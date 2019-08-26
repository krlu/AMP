package com.amp.analysis

import com.amp.analysis.ProgramTransformer.{analyzeCodeBlock, getAST, methodFilter}
import com.amp.examples.TestClass
import org.scalatest.{FlatSpec, Matchers}
import spoon.reflect.declaration.CtMethod

import scala.jdk.CollectionConverters._


class ProgramAnalysisTest extends FlatSpec with Matchers{
  "Program output Analyzer" should "output correct analysis in near constant time" in {
    val filePath = "src/main/java/com/amp/examples/TestClass.java"
    val (_, ctModel) = getAST(filePath)
    val elements = ctModel.getElements[CtMethod[Any]](methodFilter).asScala.toList
    val x = elements.head.asInstanceOf[CtMethod[Double]]

    val s1 = System.currentTimeMillis()
    val estimatedOutputs = analyzeCodeBlock(x)
    val s2 = System.currentTimeMillis()
    val runtime1 = s2 - s1

    val s3 = System.currentTimeMillis()
    val actualOutput = (new TestClass).testMethod()
    val s4 = System.currentTimeMillis()
    val runtime2 = s4 - s3

    println(estimatedOutputs)
    assert(estimatedOutputs("returnVal0") == actualOutput)
    assert(runtime1 * 100 < runtime2)
  }
}
