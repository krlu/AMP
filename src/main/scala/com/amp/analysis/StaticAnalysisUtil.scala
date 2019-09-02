package com.amp.analysis
import java.io.FileWriter

import spoon.Launcher
import spoon.reflect.CtModel
import spoon.reflect.code._
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

  def filter[T <: CtElement](c: Class[T]): TypeFilter[T] = new TypeFilter[T](c)

  /**
    * Obtains the Abstract Syntax Tree of the input java program
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

  def getRawAST[T](inputPath: String): CtType[T] = {
    val model = getAST(inputPath)
    model.getAllTypes.asScala.toList.head.asInstanceOf[CtType[T]]
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

