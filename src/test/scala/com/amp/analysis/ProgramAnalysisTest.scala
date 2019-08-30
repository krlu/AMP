package com.amp.analysis

import java.io.{File, FileWriter}

import com.amp.analysis.ProgramTransformer._
import com.amp.examples.TestClass
import org.scalatest.{FlatSpec, Matchers}
import spoon.reflect.declaration.CtMethod

import scala.io.Source._
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

    assert(estimatedOutputs("returnVal0") == actualOutput)
    assert(runtime1 * 10 < runtime2)
  }


  "Program output Analyzer" should "support refactoring method" in {
    val filePath = "src/main/java/com/amp/examples/TestClass2.java"
    val (_, ctModelOriginal) = getAST(filePath)
    val (_, ctModelCloned) = cloneAST(filePath)
    val elements = ctModelOriginal.getElements(methodFilter).asScala.toList
    val clonedElements = ctModelCloned.getElements(methodFilter).asScala.toList
    val originalMethod = elements.head
    val clonedMethod = clonedElements.head
    val blocks = ProgramTransformer.getBLocks(originalMethod.getBody)
    assert(blocks.size == 2)
    assert(blocks(0) == blocks(1))
    val helperMethods = refactorMethod(originalMethod, clonedMethod)
    assert(helperMethods.size == 1)
    helperMethods.indices.foreach { i =>
      val method = helperMethods(i)
      assert(method.getParameters.asScala.toList.map(_.getSimpleName) == List("i", "j"))
      assert(method.getSignature == s"fooHelper${i}(int,int)")
    }
    val refactoredClass = refactorMethodsForClass(filePath)
    val tempFilePath = "temp.txt"
    val fw = new FileWriter(tempFilePath)
    fw.write(refactoredClass.toString)
    fw.close()
    val testFilePath = "test.txt"
    val savedLines = fromResource(testFilePath).getLines.toList
    val buffer = fromFile(tempFilePath)
    val lines = buffer.getLines.toList
    buffer.close()
    assert(lines.size == savedLines.size)
    (lines zip savedLines).foreach{ case (l1, l2) =>
      assert(l1 == l2)
    }
    val file = new File(tempFilePath)
    file.delete()
  }
}
