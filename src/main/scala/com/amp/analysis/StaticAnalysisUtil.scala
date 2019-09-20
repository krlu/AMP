package com.amp.analysis
import java.io.{File, FileWriter}

import spoon.Launcher
import spoon.reflect.CtModel
import spoon.reflect.code._
import spoon.reflect.declaration._
import spoon.reflect.reference.CtLocalVariableReference
import spoon.reflect.visitor.filter.TypeFilter

import scala.jdk.CollectionConverters._


object StaticAnalysisUtil {

  val EMPTY_STRING = ""
  val anyFilter = filter(classOf[CtElement])
  val blockFilter = filter(classOf[CtBlock[Any]])
  val methodFilter: TypeFilter[CtMethod[Any]] = filter(classOf[CtMethod[Any]])
  val methodCallFilter = filter(classOf[CtInvocation[Any]])
  val returnFilter = filter(classOf[CtReturn[Any]])
  val assignmentFilter = filter(classOf[CtAssignment[Any, Any]])
  val localVarFilter = filter(classOf[CtLocalVariable[Any]])
  val variableFilter = filter(classOf[CtVariable[Any]])
  val variableWriteFilter = filter(classOf[CtVariableWrite[Any]])
  val variableReadFilter = filter(classOf[CtVariableRead[Any]])
  val variableReferenceFilter = filter(classOf[CtLocalVariableReference[Any]])

  def filter[T <: CtElement](c: Class[T]): TypeFilter[T] = new TypeFilter[T](c)

  /**
    * Obtains the entire compilation unit (CU) of the input java class
    * the CU contains the AST for said class as well as file meta-data including
    *  - package names
    *  - module names
    * Both can be modified with source code transformation techniques
    * @param inputPath - input file
    * @return pair consisting of
    *         cu - compilation unit, contains package info and list of imported libraries
    *         model - the AST of the input program
    */
  def getAST(inputPath: String): CtModel = {
    validateFileExtension(inputPath)
    val launcher = new Launcher
    launcher.addInputResource(inputPath)
    launcher.getEnvironment.setAutoImports(true)
    launcher.getEnvironment.setNoClasspath(true)
    launcher.buildModel
    val model: CtModel = launcher.getModel
    model
  }

  /**
    * Obtains the Abstract Syntax Tree of the input java class
    * Both can be modified with source code transformation techniques
    * @param inputPath - input file
    * @tparam T - type name represented by the class
    * @return pair consisting of
    *         cu - compilation unit, contains package info and list of imported libraries
    *         model - the AST of the input program
    */
  def getRawAST[T](inputPath: String): CtType[T] = {
    val model = getAST(inputPath)
    model.getAllTypes.asScala.toList.head.asInstanceOf[CtType[T]]
  }

  /**
    * Typically used to print out the transformed version of a class
    * @param rawSyntaxTree - raw AST contained with a model
    * @param outputPath - output file path
    */
  def printFullClass(rawSyntaxTree: CtType[_], outputPath: String, header: String = ""): Unit = {
    validateFileExtension(outputPath)
    val fw = new FileWriter(outputPath)
    fw.write(header)
    fw.write(rawSyntaxTree.toStringWithImports)
    fw.close()
  }

  private def validateFileExtension(path: String): Unit = {
    if(path.split("\\.").last != "java")
      throw new IllegalArgumentException(s"Required path with .java file extension but found ${path}")
  }

  /**
    * Creates a deep copy of the input AST element
    * Calls out to a java cloner, because the clone method is inaccessible to the scala compiler (for some reason)
    * @param ctElement - input AST element
    * @tparam T - type parameter must be some kind of CtElement
    * @return T - deep copy of input AST
    */
  def createClone[T <: CtElement](ctElement: T): T = {
    Cloner.createClone(ctElement).asInstanceOf[T]
  }
}

