package com.amp.analysis
import java.io.{File, FileWriter}

import spoon.Launcher
import spoon.reflect.CtModel
import spoon.reflect.cu.CompilationUnit
import spoon.reflect.declaration.{CtElement, CtMethod}
import spoon.reflect.visitor.filter.TypeFilter

import scala.jdk.CollectionConverters._


object ProgramAnalyzer{

  val anyFilter = new TypeFilter[CtMethod[Any]](classOf[Any])
  val methodFilter = new TypeFilter[CtMethod[Any]](classOf[CtMethod[Any]])

  def main(args: Array[String]): Unit = {
    val filePath = "src/main/java/com/amp/analysis/TestClass.java"
    val (cu, ctModel) = getAST(filePath)
    val elements = ctModel.getElements[CtMethod[Any]](methodFilter).asScala.toList
    val x = elements.head.getType
    println(x)
    printFullClass(cu, ctModel, "output.java")
  }

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
    * @param cu - compilation unit
    * @param model - current AST
    * @param outputPath - Output file
    */
  def printFullClass(cu: CompilationUnit, model: CtModel, outputPath: String): Unit = {
    val pw = new FileWriter(new File(outputPath))
    val imports = cu.getImports.asScala.toList
    pw.write(s"package ${model.getAllPackages.asScala.toList.last.toString};\n")
    imports.foreach(i => pw.write(i.toString + "\n"))
    val fullClass: CtElement = model.getAllTypes.asScala.toList.head
    pw.write(fullClass.toString)
    pw.close()
  }

  /**
    * Specifies range of possible outputs
    * @param method - AST for method
    * @tparam T - Type signature of method
    */
  def findOutputBounds[T](method: CtMethod[T]): Unit = {

  }
}
