package com.amp.analysis

import java.io.File

import com.amp.analysis.MethodRefactorer.refactorMethodsForClass
import com.amp.analysis.StaticAnalysisUtil._
import com.amp.examples.TestClass
import org.scalatest.{FlatSpec, Matchers}
import spoon.reflect.declaration.CtMethod

import scala.io.Source._
import scala.jdk.CollectionConverters._

class ProgramAnalysisTest extends FlatSpec with Matchers {

  "Program output Analyzer" should "output correct analysis in near constant time" in {
    import com.amp.analysis.MethodIOAnalyzer._
    val filePath = "src/main/java/com/amp/examples/TestClass.java"
    val ctModel = getAST(filePath)
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

    assert(estimatedOutputs("returnVal0") == actualOutput)
    assert(runtime1 * 10 < runtime2)
  }

  "Program output Analyzer" should "support refactoring method" in {
    import com.amp.analysis.MethodRefactorer._
    for(testNum <- 2 to 4) {
      val filePath = s"src/main/java/com/amp/examples/TestClass$testNum.java"
      val refactoredClass = refactorMethodsForClass(filePath)
      val tempFilePath = "temp.txt"
      printFullClass(refactoredClass, tempFilePath)
      val testFilePath = s"test$testNum.txt"
      val savedLines = fromResource(testFilePath).getLines.toList
      val buffer = fromFile(tempFilePath)
      val lines = buffer.getLines.toList
      buffer.close()
      assert(lines.size == savedLines.size)
      (lines zip savedLines).foreach {case (l1, l2) => assert(l1 == l2)}
      val file = new File(tempFilePath)
      file.delete()
    }
  }
}
