package com.amp.analysis

import com.amp.analysis.ProgramTransformer.{analyzeCodeBlock, getAST, methodFilter}
import spoon.reflect.declaration.CtMethod

import scala.jdk.CollectionConverters._

object App {

  private val usageInfo =
    """usage with flags:
      | --print [inputPath] [outputPath]
      | --analyze [inputPath]
      |""".stripMargin

  def main(args: Array[String]): Unit = {
    val flags = args(0)
    if(flags == "--help" || args.length < 2) println(usageInfo)
    else {
      val filePath = args(1)
      val (cu, ctModel) = getAST(filePath)
      val elements = ctModel.getElements[CtMethod[Any]](methodFilter).asScala.toList
      val x = elements.head.asInstanceOf[CtMethod[Double]]
      flags match {
        case "--print" =>
          val outputFilePath = args(2)
          ProgramTransformer.printFullClass(cu, ctModel, outputFilePath)
        case "--analyze" =>
          val estimatedOutputs = analyzeCodeBlock(x)
          println(estimatedOutputs)
        case _ =>
          println(usageInfo)
      }
    }
  }
}
