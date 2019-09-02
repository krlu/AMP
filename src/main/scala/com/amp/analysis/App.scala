package com.amp.analysis

import com.amp.analysis.MethodRefactorer.refactorMethodsForClass
import com.amp.analysis.StaticAnalysisUtil._
import spoon.reflect.declaration.CtMethod

import scala.jdk.CollectionConverters._

object App {

  private val usageInfo =
    """usage with flags:
      | --print [inputPath] [outputPath]
      | --analyze [inputPath]
      | --refactor [inputPath] [outputPath]
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val flags = args(0)
    if(flags == "--help" || args.length < 2) println(usageInfo)
    else {
      val filePath = args(1)
      val ctModel = getAST(filePath)
      val elements = ctModel.getElements[CtMethod[Any]](methodFilter).asScala.toList
      val x = elements.head.asInstanceOf[CtMethod[Double]]
      flags match {
        case "--refactor" =>
          val outputFilePath = args(2)
          val refactoredClass = refactorMethodsForClass(filePath)
          printFullClass(refactoredClass, outputFilePath)
        case "--print" =>
          val outputFilePath = args(2)
          printFullClass(ctModel.getAllTypes.asScala.head, outputFilePath)
        case "--analyze" =>
          val estimatedOutputs = MethodIOAnalyzer.analyzeCodeBlock(x)
          println(estimatedOutputs)
        case _ =>
          println(usageInfo)
      }
    }
  }
}
