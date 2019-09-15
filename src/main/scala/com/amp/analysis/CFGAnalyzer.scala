package com.amp.analysis

import com.amp.analysis.StaticAnalysisUtil.methodFilter
import spoon.reflect.code.{CtAssignment, CtBlock}
import spoon.reflect.declaration.CtElement

import scala.jdk.CollectionConverters._


object CFGAnalyzer {

  def main(args: Array[String]): Unit = {
    val path = "src/test/java/com/amp/examples/dependencygraph/TestClass1.java"
    val model = StaticAnalysisUtil.getAST(path)
    val method = model.getElements(methodFilter).asScala.toList.head
    println(method.getType)
    method.getBody.getDirectChildren.asScala.toList.foreach{ x =>
      println(x.getClass)
    }
  }
  /**
    * Analyze the state of the program before and after applying a block
    * @param cfg - state pre block application
    * @param block - code block to be applied
    * @return state post block application
    */
  def applyBlock(cfg: ControlFlowGraph, block: CtBlock[Any]): ControlFlowGraph = {
    block.getDirectChildren.asScala.toList.map{ x: CtElement =>
      if(Recognizer.recognize[CtAssignment[Any, Any]](x)){
        val a = x.asInstanceOf[CtAssignment[Any, Any]]
        a.getAssigned
      }
      null
    }
    null
  }
}
