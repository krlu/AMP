package com.amp.analysis

import java.io.{File, FileWriter}

import com.amp.analysis.StaticAnalysisUtil._
import spoon.reflect.code.{CtBlock, CtExpression, CtFieldRead, CtIf, CtInvocation, CtLocalVariable, CtLoop, CtVariableAccess}
import spoon.reflect.declaration.{CtExecutable, CtMethod, CtNamedElement, CtParameter, CtType, ModifierKind}
import spoon.reflect.factory.Factory
import spoon.reflect.reference.{CtTypeReference, CtVariableReference}
import spoon.support.reflect.code.CtVariableWriteImpl
import spoon.support.reflect.reference.CtLocalVariableReferenceImpl

import scala.jdk.CollectionConverters._

/**
  * Specifically for handling method refactoring within one class or object
  */
object MethodRefactorer {
  /**
    * Creates a deep copy of the source code and refactors the copy
    * @param filePath - path to input source code file
    * @return - refactored source code
    */
  def refactorMethodsForClass(filePath: String): Option[CtType[_]] = {
    var fileNum = 0
    var outPath = filePath
    var x: Option[CtType[_]]= None
    while(MethodRefactorer.isRefactorable(outPath)){
      x = Some(oneIterationRefactor(outPath))
      outPath = s"temp${fileNum}.java"
      StaticAnalysisUtil.printFullClass(x.get, outPath)
      fileNum += 1
    }
    for(i <- 0 to fileNum){
      val fw = new File(s"temp${i}.java")
      fw.delete()
    }
    x
  }

  def oneIterationRefactor(filePath: String): CtType[_] = {
    val ctModelOriginal = getAST(filePath)
    val ctModelCloned = getAST(filePath)
    val methods = ctModelOriginal.getElements(methodFilter).asScala.toList
    val clonedMethods = ctModelCloned.getElements(methodFilter).asScala.toList
    val helperMethods = (methods zip clonedMethods).flatMap { case (a, b) => refactorMethod[Any](a, b, methods) }
    val clonedCode = ctModelCloned.getAllTypes.asScala.head
    val methodCalls = clonedCode.getElements(methodCallFilter).asScala.toList.map(_.getExecutable.getSimpleName)
    helperMethods.foreach(clonedCode.addMethod)
    helperMethods.filterNot{ method =>
      val name = method.getSimpleName
      methodCalls.contains(name)
    }.foreach(clonedCode.removeMethod)
    clonedCode
  }

  /** Searches sub blocks for recurring code structures
    * Turns recurring code into separate method and replaces recurring with new method call
    * @param method - original method
    * @param methodClone - method clone (this is the method we will refactor)
    * @return - refactored helper functions
    */
  private def refactorMethod[T](method: CtMethod[T], methodClone: CtMethod[T], allMethods: List[CtMethod[T]]): List[CtMethod[T]] = {
    val blocks = getBlocks[T](method.getBody)
    val clonedBlocks = getBlocks[T](methodClone.getBody)
    val uniqueBlocks = blocks.distinct
    val uniqueClonedBlocks = clonedBlocks.distinct
    val uniqueMultiBlocks = uniqueBlocks.filter{ block => blocks.count(_ == block) > 1 && !isSingleMethod(block)}
    val uniqueClonedMultiBlocks = uniqueClonedBlocks.filter{ block => blocks.count(_ == block) > 1 && !isSingleMethod(block)}
    val blockToMethod: Map[String, CtMethod[T]] = uniqueMultiBlocks.indices.map{ i =>
      val block = uniqueClonedMultiBlocks(i)
      // get all variables defined in block
      val innerScopeVars = block.getElements(localVarFilter).asScala.toList.map(_.getReference.getSimpleName)
      // get all variables defined or used in block and outside block
      val allScopeVars = List(variableReferenceFilter, variableReadFilter, variableWriteFilter)
        .flatMap(block.getElements(_).asScala.toList)
        .filterNot(e => Recognizer.recognize[CtFieldRead[T]](e))
      // filter out variables defined in block, remaining variables to be used as params to new method
      val relevantVarNames: List[(CtTypeReference[T], String)] = allScopeVars
        .filterNot(e => innerScopeVars.contains(e.toString))
        .map(e => (e.getReferencedTypes.asScala.toList.head.asInstanceOf[CtTypeReference[T]], e.toString))
        .distinct.sortBy(_._2)
      block.toString -> {
        // check if desired helper method already exists, if so use that, otherwise create brand new one
        val existingMethodOpt = allMethods.find(_.getBody == uniqueMultiBlocks(i))
        existingMethodOpt match {
          case Some(m) => m
          case None =>  createHelperMethod[T](uniqueMultiBlocks(i), relevantVarNames,
            method.getType, s"${method.getSimpleName}Helper$i")
        }
      }
    }.toMap
    clonedBlocks.indices.foreach{ i =>
      val clonedBlock = clonedBlocks(i)
      val originalBlock = blocks(i)
      if(blockToMethod.contains(clonedBlock.toString)) {
        updateBlocks(clonedBlock, originalBlock, blockToMethod(clonedBlock.toString))
      }
    }
    blockToMethod.values.toList
  }

  /**
    * Replace block contents with method call to helper method
    * @param clonedBlock - cloned block to be modified
    * @param originalBlock - original block, contains references to original variables in scope
    * @param helperMethod - new helper method
    */
  private def updateBlocks[T](clonedBlock: CtBlock[T], originalBlock: CtBlock[T], helperMethod: CtMethod[T]): Unit = {
    clonedBlock.getStatements.asScala.toList.foreach(clonedBlock.removeStatement)
    val factory = clonedBlock.getFactory
    val executable = factory.createExecutableReference()
    executable.setSimpleName(helperMethod.getSimpleName)
    val methodCall = clonedBlock.getFactory.createInvocation()
    methodCall.setExecutable(executable)
    val variables = helperMethod.getParameters.asScala.toList.map{ param =>
      val varRef: CtVariableReference[T] = new CtLocalVariableReferenceImpl()
      varRef.setSimpleName(param.getSimpleName)
      val varWrite = new CtVariableWriteImpl[T]()
      varWrite.setVariable(varRef)
      varWrite
    }
    variables.foreach(methodCall.addArgument)
    clonedBlock.addStatement(methodCall)
  }

  /**
    * @param block - original block
    * @param relevantVarNames - variables that become arguments to the new method. contains variable type and name
    * @param returnType - return type of helper method
    * @param helperMethodName - name of helperMethod
    * @return new helper method containing original block and parameters as variables defined outside the block
    */
  private def createHelperMethod[T](block: CtBlock[T], relevantVarNames: List[(CtTypeReference[T], String)],
                                    returnType: CtTypeReference[T], helperMethodName: String): CtMethod[T] = {
    val factory: Factory = block.getFactory
    val method: CtMethod[T] = factory.createMethod()
    method.setSimpleName(helperMethodName)
    relevantVarNames.foreach{ case (typeRef, name) =>
      val param: CtParameter[T] = factory.createParameter[T]()
      param.setSimpleName(name)
      param.setType(typeRef)
      method.addParameter[CtExecutable[T]](param)
    }
    if(returnType.getSimpleName != "void") {
      var paramsToReturn = List.empty[CtVariableReference[T]]
      block.getElements(assignmentFilter).asScala.toList.foreach { a =>
        paramsToReturn = paramsToReturn :+ a.getAssigned.asInstanceOf[CtVariableAccess[T]].getVariable
      }
      val returnStatement = factory.createReturn[T]()
      val variables = factory.createLiteralArray(paramsToReturn.toArray).asInstanceOf[CtExpression[T]]
      returnStatement.setReturnedExpression(variables)
      block.addStatement(returnStatement)
    }
    method.setBody(block)
    method.addModifier(ModifierKind.PRIVATE)
    method.setType(returnType)
    method
  }

  /**
    * Finds all code blocks in a given method, used to support refactoring
    * @param codeBlock - input source code
    * @return list of CtBlock
    */
  private def getBlocks[T](codeBlock: CtBlock[T], isRoot: Boolean = true): List[CtBlock[T]] = {
    var blocks: List[CtBlock[T]] = codeBlock.getDirectChildren.asScala.toList.flatMap { x =>
      if (Recognizer.recognize[CtIf](x)) {
        val ifStatement = x.asInstanceOf[CtIf]
        val thenBlock = ifStatement.getThenStatement.asInstanceOf[CtBlock[T]]
        val elseBlock = ifStatement.getElseStatement.asInstanceOf[CtBlock[T]]
        List(thenBlock, elseBlock).filter(_ != null).flatMap { b =>
          if (isChildBlock(b)) List(b) else getBlocks[T](b, isRoot = false)
        }
      }
      else if (Recognizer.recognize[CtLoop](x)) {
        val loopStatement = x.asInstanceOf[CtLoop]
        val loopBlock = loopStatement.getBody.asInstanceOf[CtBlock[T]]
        if (isChildBlock(loopBlock)) List(loopBlock) else getBlocks(loopBlock, isRoot = false)
      }
      else if (Recognizer.recognize[CtBlock[T]](x)) {
        val block = x.asInstanceOf[CtBlock[T]]
        if (isChildBlock(block)) List(block) else getBlocks(block, isRoot = false)
      } else List.empty[CtBlock[T]]
    }
    if(!isRoot)
      blocks = List(codeBlock) ++ blocks
    reNameLocalVars(blocks)
    blocks
  }

  /**
    * For changing variables to be the same name across all blocks
    * This way, we can find blocks that are logically identical despite being syntactically different
    * @param blocks - input code blocks
    */
  private def reNameLocalVars[T](blocks: Seq[CtBlock[T]]): Unit = {
    val localVarNames = blocks.flatMap{ b => b.getElements(localVarFilter).asScala.toList.map(_.getSimpleName)}
    blocks.foreach{ b: CtBlock[T] =>
      val localVars = b.getElements(filter(classOf[CtLocalVariable[T]])).asScala.toList
      localVars.indices.foreach{ i =>
        val localVar: CtLocalVariable[T] = localVars(i)
        val oldName = localVar.getSimpleName
        val newName = localVarNames(i)
        localVar.setSimpleName[CtNamedElement](newName)
        b.getElements(variableReferenceFilter).asScala.toList.foreach{ varRef =>
          if(varRef.getSimpleName == oldName) varRef.setSimpleName(newName)
        }
      }
    }
  }

  /**
    * Checks if input block has any child elements (excluding itself) that are of type CtBlock
    * Thus if the set of children is 1 or 0, return true otherwise return false
    * @param codeBlock - input code block
    * @return - true if and only if input block is child block
    */
  private def isChildBlock[T](codeBlock: CtBlock[T]): Boolean =
    codeBlock.getElements(blockFilter).asScala.toList.size <= 1

  /**
    * Checks if block only has one direct child and checks if that child is of type CtMethod
    * @param codeBlock - input code block
    * @return true if has no children or a single method call
    */
  private def isSingleMethod[T](codeBlock: CtBlock[T]): Boolean = {
    val children = codeBlock.getDirectChildren
    children.isEmpty || children.size() == 1 && Recognizer.recognize[CtInvocation[T]](children.get(0))
  }

  def isRefactorable(filePath: String): Boolean = {
    val ctModelOriginal = getAST(filePath)
    ctModelOriginal.getElements(methodFilter).asScala.toList.exists(isRefactorableMethod)
  }

  private def isRefactorableMethod[T](method: CtMethod[T]): Boolean = {
    val blocks = getBlocks[T](method.getBody)
    val uniqueBlocks = blocks.distinct
    val uniqueMultiBlocks = uniqueBlocks.filter{ block => blocks.count(_ == block) > 1 && !isSingleMethod(block)}
    uniqueMultiBlocks.nonEmpty
  }
}
