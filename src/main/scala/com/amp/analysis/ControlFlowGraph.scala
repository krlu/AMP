package com.amp.analysis

import spoon.reflect.code.CtBlock
import spoon.reflect.declaration.CtElement

class ControlFlowGraph(nodes: Map[CtElement, Set[CtElement]])

object ControlFlowGraph{
  def apply(nodes: Map[CtElement, Set[CtElement]]): ControlFlowGraph = new ControlFlowGraph(nodes)
  def apply[T](block: CtBlock[T]): ControlFlowGraph = ???
}
