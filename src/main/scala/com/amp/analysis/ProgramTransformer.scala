package com.amp.analysis
import java.io.{File, FileWriter}

import spoon.Launcher
import spoon.reflect.CtModel
import spoon.reflect.code._
import spoon.reflect.cu.CompilationUnit
import spoon.reflect.declaration.{CtElement, CtMethod}
import spoon.reflect.visitor.filter.TypeFilter

import scala.jdk.CollectionConverters._


object ProgramTransformer{

  val methodFilter = filter(classOf[CtMethod[Any]])
  val returnFilter = filter(classOf[CtReturn[Any]])
  val assignmentFilter = filter(classOf[CtAssignment[Any, Any]])
  val localVarFilter = filter(classOf[CtLocalVariable[Any]])

  private def filter[T <: CtElement](c: Class[T]): TypeFilter[T] = new TypeFilter[T](c)

  /**
    * Obtains the Abstract Syntax Tree and Compilation Unit of the input java program
    * Both can be modified with source code transformation techniques
    * @param inputPath - input file
    * @return pair consisting of
    *         cu - compilation unit, contains package info and list of imported libraries
    *         model - the AST of the input program
    */
  def getAST(inputPath: String): (CompilationUnit, CtModel) ={
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
          val methodCall = statement.asInstanceOf[CtInvocation[Any]].getExecutable
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
  def handleReturnStatement(statement: CtReturn[Double], scopeVariables: Map[String, Double] = Map.empty[String, Double]):
  (List[Double], List[(Double, Double)]) = {
    var constants: List[Double] = List.empty[Double]
    val bounds: List[(Double, Double)] = List.empty[(Double, Double)]
    val child = statement.getDirectChildren.get(0)
    // find all return statements with literals
    if(Recognizer.recognize[CtLiteral[Double]](child))
      constants = constants ++ List(child.asInstanceOf[CtLiteral[Double]].getValue)
    // find all return statements with variable references
    if(Recognizer.recognize[CtVariableRead[Double]](child)){
      if(scopeVariables.contains(child.toString))
        constants = constants :+ scopeVariables(child.toString)
    }
    (constants, bounds)
  }
}

