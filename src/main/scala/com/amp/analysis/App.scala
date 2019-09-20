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
      flags match {
        case "--refactor" =>
          val outputFilePath = args(2)
          val (refactoredClass, header) = refactorMethodsForClass(filePath)
          refactoredClass match {
            case Some(rfc) => printFullClass(rfc, outputFilePath, header)
            case None =>
              System.out.println("Class was not refactorable!")
          }
        case "--print" =>
          val outputFilePath = args(2)
          printFullClass(ctModel.getAllTypes.asScala.head, outputFilePath)
        case "--analyze" =>
          ctModel.getElements[CtMethod[Any]](methodFilter).asScala.toList.foreach { method =>
            val estimatedOutputs = MethodIOAnalyzer.analyzeCodeBlock(method)
            println(estimatedOutputs)
          }
        case _ =>
          println(usageInfo)
      }
    }
  }
}
