package com.amp.analysis

import java.io.File

import com.amp.analysis.StaticAnalysisUtil._
import com.amp.examples.ioestimation.TestClass
import org.scalatest.{FlatSpec, Matchers}
import spoon.reflect.declaration.CtMethod

import scala.io.Source._
import scala.jdk.CollectionConverters._

class ProgramAnalysisTest extends FlatSpec with Matchers {

  private val inputTestPath = "src/test/java/com/amp/examples"

  "Program output Analyzer" should "output correct analysis in near constant time" in {
    import com.amp.analysis.MethodIOAnalyzer._
    val filePath = s"$inputTestPath/ioestimation/TestClass.java"
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
    for(testNum <- 1 to 7) {
      val filePath = s"$inputTestPath/refactor/TestClass$testNum.java"
      val (refactoredClass, header) = refactorMethodsForClass(filePath)
      val tempFilePath = "temp.java"
      if(refactoredClass.nonEmpty)
        printFullClass(refactoredClass.get, tempFilePath, header)
      val testFileName = s"refactor/test$testNum.java"
      val testFilePath = s"src/test/resources/$testFileName"
      val savedLines = fromResource(testFileName).getLines.toList
      val buffer = fromFile(tempFilePath)
      val lines = buffer.getLines.toList
      buffer.close()
      val m1 = getAST(testFilePath).getAllTypes.asScala.toList.head
      val m2 = getAST(tempFilePath).getAllTypes.asScala.toList.head
//      assert(m1 == m2)
      assert(m1.toStringWithImports == m2.toStringWithImports)
      assert(lines.size == savedLines.size)
      (lines zip savedLines).foreach {case (l1, l2) =>
        assert(l1.trim == l2.trim)
      }
      val file = new File(tempFilePath)
      file.delete()
    }
  }
}
