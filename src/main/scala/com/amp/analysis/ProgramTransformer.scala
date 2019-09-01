package com.amp.analysis
import java.io.{File, FileWriter}

import spoon.Launcher
import spoon.reflect.CtModel
import spoon.reflect.code._
import spoon.reflect.cu.CompilationUnit
import spoon.reflect.declaration._
import spoon.reflect.factory.Factory
import spoon.reflect.reference.{CtLocalVariableReference, CtTypeReference, CtVariableReference}
import spoon.reflect.visitor.filter.TypeFilter
import spoon.support.reflect.code.CtVariableWriteImpl
import spoon.support.reflect.reference.CtLocalVariableReferenceImpl

import scala.jdk.CollectionConverters._


object ProgramTransformer{

  val anyFilter = filter(classOf[CtElement])
  val methodFilter = filter(classOf[CtMethod[Any]])
  val returnFilter = filter(classOf[CtReturn[Any]])
  val assignmentFilter = filter(classOf[CtAssignment[Any, Any]])
  val localVarFilter = filter(classOf[CtLocalVariable[Any]])
  val variableFilter = filter(classOf[CtVariable[Any]])
  val variableWriteFilter = filter(classOf[CtVariableWrite[Any]])
  val variableReadFilter = filter(classOf[CtVariableRead[Any]])
  val variableReferenceFilter = filter(classOf[CtLocalVariableReference[Any]])

  private def filter[T <: CtElement](c: Class[T]): TypeFilter[T] = new TypeFilter[T](c)

  def cloneAST(inputPath: String): (CompilationUnit, CtModel) = {
    val (cu, model) = getAST(inputPath)
    val pathToCopy = s"${inputPath.split("/").last.replace(".java", "_copy.java")}"
    printFullClass(cu, model, pathToCopy)
    val ast = getAST(pathToCopy)
    val file = new File(pathToCopy)
    file.delete()
    ast
  }

  /**
    * Obtains the Abstract Syntax Tree and Compilation Unit of the input java program
    * Both can be modified with source code transformation techniques
    * @param inputPath - input file
    * @return pair consisting of
    *         cu - compilation unit, contains package info and list of imported libraries
    *         model - the AST of the input program
    */
  def getAST(inputPath: String): (CompilationUnit, CtModel) = {
    val launcher = new Launcher
    launcher.addInputResource(inputPath)
    launcher.getEnvironment.setAutoImports(true)
    launcher.getEnvironment.setNoClasspath(true)
    launcher.buildModel
    val model: CtModel = launcher.getModel
    val cu = launcher.getFactory.CompilationUnit()
      .getOrCreate(model.getAllTypes.asScala.head.getPosition.getFile.getAbsolutePath)
    (cu, model)
  }

  /**
    * Typically used to print out the transformed version of a class
    * @param cu - compilation unit
    * @param model - current AST
    * @param outputPath - Output file
    */
  def printFullClass(cu: CompilationUnit, model: CtModel, outputPath: String): Unit = {
    val pw = new FileWriter(new File(outputPath))
    val imports = cu.getImports.asScala.toList
    pw.write(s"package ${model.getAllPackages.asScala.toList.last.toString};\n")
    imports.foreach(i => pw.write(i.toString + "\n"))
    pw.write("\n")
    val fullClass: CtElement = model.getAllTypes.asScala.toList.head
    pw.write(fullClass.toString)
    pw.close()
  }

  /**
    * @param holder - holder of code block
    * @param scopeVariables - variables in scope prior to executing this code block
    * @return output variables - variables in scope upon output
    *         - If block executes a return statement, then the output variables is Map("returnVarName", returnValue)
    */
  def analyzeCodeBlock(holder: CtBodyHolder, scopeVariables: Map[String, Double] = Map.empty[String, Double]): Map[String, Double] = {
    var toReturnVars = scopeVariables
    if(Recognizer.recognize[CtFor](holder)) {
      val forBlock = holder.asInstanceOf[CtFor]
      val ((initVarName, initValue), exitValue, updateOperators) = getForLoopSignature(forBlock)
      val isPostInc = updateOperators.getKind == UnaryOperatorKind.POSTINC
      forBlock.getBody.asInstanceOf[CtBlock[Double]].forEach { statement =>
        if(Recognizer.recognize[CtInvocation[AnyVal]](statement)) {
//          val methodCall = statement.asInstanceOf[CtInvocation[Any]].getExecutable
        }
        if (Recognizer.recognize[CtOperatorAssignment[Any, Any]](statement)) {
          val assignment = statement.asInstanceOf[CtOperatorAssignment[Any, Any]]
          val assignedVar = assignment.getAssigned.toString
          if (assignment.getAssignment.toString == initVarName && scopeVariables.contains(assignedVar) && isPostInc)
            toReturnVars = toReturnVars ++ Map(assignedVar -> (exitValue - initValue + 1) * (exitValue + initValue) / 2)
        }
        if(Recognizer.recognize[CtReturn[Double]](statement)){
          val (constants, _) = handleReturnStatement(statement.asInstanceOf[CtReturn[Double]], toReturnVars)
          return toReturnVars ++ constants.indices.map{ i => s"returnVal$i" -> constants(i)}
        }
        if(Recognizer.recognize[CtBodyHolder](statement)) {
          toReturnVars = toReturnVars ++ analyzeCodeBlock(statement.asInstanceOf[CtBodyHolder], toReturnVars)
          if(toReturnVars.keySet.exists(k => k.contains("returnVal")))
            return toReturnVars
        }
      }
    }
    if(Recognizer.recognize[CtMethod[Double]](holder)){
      val method = holder.asInstanceOf[CtMethod[Double]]
      val body: CtBlock[Double] = method.getBody
      body.forEach{ statement =>
        if(Recognizer.recognize[CtLocalVariable[Double]](statement)){
          val localVar = statement.asInstanceOf[CtLocalVariable[Double]]
          val assignedVal = localVar.getAssignment.asInstanceOf[CtLiteral[Double]].getValue
          toReturnVars = toReturnVars ++ Map(localVar.getReference.getSimpleName -> assignedVal)
        }
        if(Recognizer.recognize[CtAssignment[Double, Double]](statement)){
          val localAssign = statement.asInstanceOf[CtAssignment[Double, Double]]
          val varName = localAssign.getAssigned.toString
          val assignValue = localAssign.getAssignment.asInstanceOf[CtLiteral[Double]].getValue
          if(toReturnVars.contains(varName))
            toReturnVars = toReturnVars ++ Map(varName -> assignValue)
        }
        if(Recognizer.recognize[CtBodyHolder](statement)) {
          toReturnVars = toReturnVars ++ analyzeCodeBlock(statement.asInstanceOf[CtBodyHolder], toReturnVars)
          if(toReturnVars.keySet.exists(k => k.contains("returnVal")))
            return toReturnVars
        }
        if(Recognizer.recognize[CtReturn[Double]](statement)){
          val (constants, _) = handleReturnStatement(statement.asInstanceOf[CtReturn[Double]], toReturnVars)
          toReturnVars = toReturnVars ++ constants.indices.map{ i => s"returnVal$i" -> constants(i)}
        }
      }
    }
    toReturnVars
  }

  /**
    * @param forBlock - for loop block
    * @return tuple consisting of ((initialVarName, initialValue, exitValue, updateOperator))
    */
  def getForLoopSignature(forBlock: CtFor): ((String, Int), Double, CtUnaryOperator[Double]) = {
    val initialState = forBlock.getForInit.get(0).asInstanceOf[CtLocalVariable[Int]]
    val initVarName = initialState.getReference.getSimpleName
    val initValue = initialState.getAssignment.asInstanceOf[CtLiteral[Int]].getValue
    val exitExpression = forBlock.getExpression.asInstanceOf[CtBinaryOperator[Any]]
    val exitValueBound = exitExpression.getRightHandOperand.asInstanceOf[CtLiteral[Int]].getValue
    val exitValue: Double = exitExpression.getKind match {
      case BinaryOperatorKind.LT => exitValueBound - 1
      case BinaryOperatorKind.LE => exitValueBound
      case _ => throw new IllegalStateException(s"binary operator must be < or <=, but was ${exitExpression.getKind}")
    }
    val update = forBlock.getForUpdate.get(0).asInstanceOf[CtUnaryOperator[Double]]
    ((initVarName, initValue), exitValue, update)
  }

  /**
    * Specifies range of possible outputs
    *
    * @param statement - AST for return statement
    * @param scopeVariables - variables within scope of this returns statement
    * @return (List[Double], List[(Double,Double)) - Possible Outputs
    */
  def handleReturnStatement[T](statement: CtReturn[T], scopeVariables: Map[String, T] = Map.empty[String, T]):
  (List[T], List[(T, T)]) = {
    var constants: List[T] = List.empty[T]
    val bounds: List[(T, T)] = List.empty[(T, T)]
    val child = statement.getDirectChildren.get(0)
    // find all return statements with literals
    if(Recognizer.recognize[CtLiteral[T]](child))
      constants = constants ++ List(child.asInstanceOf[CtLiteral[T]].getValue)
    // find all return statements with variable references
    if(Recognizer.recognize[CtVariableRead[T]](child)){
      if(scopeVariables.contains(child.toString))
        constants = constants :+ scopeVariables(child.toString)
    }
    (constants, bounds)
  }

  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
  //////////////////////////////////////// Methods for handling refactoring ////////////////////////////////////////////
  //////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

  /**
    * Creates a deep copy of the source code and refactors the copy
    * @param filePath - path to input source code file
    * @return - refactored source code
    */
  def refactorMethodsForClass(filePath: String): CtType[_] = {
    val (_, ctModelOriginal) = getAST(filePath)
    val (_, ctModelCloned) = cloneAST(filePath)
    val methods = ctModelOriginal.getElements(methodFilter).asScala.toList
    val clonedMethods = ctModelCloned.getElements(methodFilter).asScala.toList
    val helperMethods = (methods zip clonedMethods).flatMap { case (a, b) => refactorMethod[Any](a, b) }
    val clonedCode = ctModelCloned.getAllTypes.asScala.head
    helperMethods.foreach(clonedCode.addMethod)
    clonedCode
  }

  /**
    * @param method - original method
    * @param methodClone - method clone (this is the method we will refactor)
    * @return - refactored helper functions
    */
  def refactorMethod[T](method: CtMethod[T], methodClone: CtMethod[T]): List[CtMethod[T]] = {
    val blocks = getBLocks[T](method.getBody)
    val clonedBlocks = getBLocks[T](methodClone.getBody)
    val uniqueBlocks = blocks.distinct
    val uniqueMultiBlocks = uniqueBlocks.filter { block => blocks.count(_ == block) > 1 }
    val uniqueClonedBlocks = clonedBlocks.distinct
    val blockToMethod: Map[String, CtMethod[T]] = uniqueMultiBlocks.indices.map{ i =>
      val block = uniqueClonedBlocks(i)
      val innerScopeVars = block.getElements(localVarFilter).asScala.toList.map(_.getReference.getSimpleName)
      val allScopeVars = List(variableReferenceFilter, variableReadFilter, variableWriteFilter)
        .flatMap(block.getElements(_).asScala.toList)
        .filterNot(e => Recognizer.recognize[CtFieldRead[T]](e))
      val relevantVarNames: List[(CtTypeReference[T], String)] = allScopeVars
        .filterNot(e => innerScopeVars.contains(e.toString))
        .map(e => (e.getReferencedTypes.asScala.toList.head.asInstanceOf[CtTypeReference[T]], e.toString))
        .distinct.sortBy(_._2)
      block.toString -> createHelperMethod[T](uniqueMultiBlocks(i), relevantVarNames, method.getType, s"${method.getSimpleName}Helper$i")
    }.toMap
    clonedBlocks.indices.foreach{ i =>
      val clonedBlock = clonedBlocks(i)
      val originalBlock = blocks(i)
      updateBlocks(clonedBlock, originalBlock, blockToMethod(clonedBlock.toString))
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
    * @param relevantVarNames - arguments for helper method
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
    * Finds all code blocks in a give method, used to support refactoring
    * @param codeBlock - input source code
    * @return list of CtBlock
    */
  private def getBLocks[T](codeBlock: CtBlock[T]): List[CtBlock[T]] = {
    codeBlock.getDirectChildren.asScala.toList.flatMap { x =>
      if (Recognizer.recognize[CtIf](x)) {
        val ifStatement = x.asInstanceOf[CtIf]
        val thenBlock = ifStatement.getThenStatement.asInstanceOf[CtBlock[T]]
        val elseBlock = ifStatement.getElseStatement.asInstanceOf[CtBlock[T]]
        List(thenBlock, elseBlock).filter(_ != null)
      }
      else if (Recognizer.recognize[CtLoop](x)) {
        val loopStatement = x.asInstanceOf[CtLoop]
        List(loopStatement.getBody.asInstanceOf[CtBlock[T]])
      }
      else if (Recognizer.recognize[CtBlock[T]](x)) List(x.asInstanceOf[CtBlock[T]])
      else List.empty[CtBlock[T]]
    }
  }
}

