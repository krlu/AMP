package com.amp.analysis
import java.io.{File, FileWriter}

import spoon.Launcher
import spoon.reflect.CtModel
import spoon.reflect.code._
import spoon.reflect.cu.CompilationUnit
import spoon.reflect.declaration._
import spoon.reflect.reference.CtLocalVariableReference
import spoon.reflect.visitor.filter.TypeFilter

import scala.jdk.CollectionConverters._


object StaticAnalysisUtil {

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

  /** Creates a temporary file with the exact same AST as the original source code
    * Reads the AST from that file then deletes that file
    * This roundabout way of deep copying was due to inability to clone CtElements directly
    * @param inputPath - input file
    * @return pair consisting of
    *         cu - compilation unit, contains package info and list of imported libraries
    *         model - the AST of the input program
    */
  def cloneAST(inputPath: String): CtModel = {
    val model = getAST(inputPath)
    val pathToCopy = s"${inputPath.split("/").last.replace(".java", "_copy.java")}"
    printFullClass(model.getAllTypes.asScala.head, pathToCopy)
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
  def getAST(inputPath: String): CtModel = {
    val launcher = new Launcher
    launcher.addInputResource(inputPath)
    launcher.getEnvironment.setAutoImports(true)
    launcher.getEnvironment.setNoClasspath(true)
    launcher.buildModel
    val model: CtModel = launcher.getModel
    model
  }

  /**
    * Typically used to print out the transformed version of a class
    * @param rawSyntaxTree - raw AST contained with a model
    * @param outputPath - output file path
    */
  def printFullClass(rawSyntaxTree: CtType[_], outputPath: String): Unit = {
    val fw = new FileWriter(outputPath)
    fw.write(rawSyntaxTree.toStringWithImports)
    fw.close()
  }
}

