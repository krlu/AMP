package com.amp.analysis

import spoon.reflect.code.{CtAssignment, CtBlock}
import spoon.reflect.declaration.{CtElement, CtType}

import scala.jdk.CollectionConverters._


object CFGAnalyzer {

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
