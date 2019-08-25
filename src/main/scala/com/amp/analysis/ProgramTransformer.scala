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
    * Specifies range of possible outputs
    * @param method - AST for method
    * @return (List[Double], List[(Double,Double)) - Possible Outputs
    */
  def findOutputBounds(method: CtMethod[Double]): (List[Double], List[(Double, Double)]) = {
    val body: CtBlock[Double] = method.getBody
    var scopeVariables = Map.empty[String, Double]

    // TODO: currently limited to numerical for loops
    body.forEach{ x =>
      if(Recognizer.recognize[CtLocalVariable[Double]](x)){
        val localVar = x.asInstanceOf[CtLocalVariable[Double]]
        val assignedVal = localVar.getAssignment.asInstanceOf[CtLiteral[Double]].getValue
        scopeVariables = scopeVariables ++ Map(localVar.getReference.getSimpleName -> assignedVal)
      }
      if(Recognizer.recognize[CtAssignment[Double, Double]](x)){
        val localAssign = x.asInstanceOf[CtAssignment[Double, Double]]
        val varName = localAssign.getAssigned.toString
        val assignValue = localAssign.getAssignment.asInstanceOf[CtLiteral[Double]].getValue
        if(scopeVariables.contains(varName))
          scopeVariables = scopeVariables ++ Map(varName -> assignValue)
      }
      if(Recognizer.recognize[CtFor](x)) {
        val forBlock = x.asInstanceOf[CtFor]
        val initialState = forBlock.getForInit.get(0).asInstanceOf[CtLocalVariable[Int]]
        val initVarName = initialState.getReference.getSimpleName
        initialState.getAssignment
        val initValue = initialState.getAssignment.asInstanceOf[CtLiteral[Int]].getValue

        val exitExpression = forBlock.getExpression.asInstanceOf[CtBinaryOperator[Any]]
        val exitValueBound = exitExpression.getRightHandOperand.asInstanceOf[CtLiteral[Int]].getValue
        val exitValue: Double = exitExpression.getKind match {
          case BinaryOperatorKind.LT => exitValueBound - 1
          case BinaryOperatorKind.LE => exitValueBound
          case _ => throw new IllegalStateException(s"binary operator must be < or <=, but was ${exitExpression.getKind}")
        }
        val update = forBlock.getForUpdate.get(0).asInstanceOf[CtUnaryOperator[Double]]
        val isPostInc = update.getKind == UnaryOperatorKind.POSTINC
        forBlock.getBody.asInstanceOf[CtBlock[Double]].forEach{ y =>
          if(Recognizer.recognize[CtOperatorAssignment[Any, Any]](y)) {
            val assignment = y.asInstanceOf[CtOperatorAssignment[Any, Any]]
            val assignedVar = assignment.getAssigned.toString
            if (assignment.getAssignment.toString == initVarName && scopeVariables.contains(assignedVar) && isPostInc) {
              scopeVariables = scopeVariables ++ Map(assignedVar -> (exitValue - initValue + 1) * (exitValue + initValue) / 2)
            }
          }
        }
      }
    }

    var constants: List[Double] = List.empty[Double]
    val bounds: List[(Double, Double)] = List.empty[(Double, Double)]

    val returnStatements = body.getElements(returnFilter).asScala.toList
    returnStatements.foreach{ statement =>
      val child = statement.getDirectChildren.get(0)
      // find all return statements with literals
      if(Recognizer.recognize[CtLiteral[Double]](child))
        constants = constants ++ List(child.asInstanceOf[CtLiteral[Double]].getValue)
      // find all return statements with variable references
      if(Recognizer.recognize[CtVariableRead[Double]](child)){
        if(scopeVariables.contains(child.toString))
          constants = constants :+ scopeVariables(child.toString)
      }
    }
    (constants, bounds)
  }
}

