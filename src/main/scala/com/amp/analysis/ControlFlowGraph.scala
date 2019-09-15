package com.amp.analysis

import spoon.reflect.code.CtLiteral

class ControlFlowGraph(nodes: Map[State, Set[State]])

/**
  * Represents the state of the program
  */
case class State(variablesInScope: Map[String, CtLiteral[Any]])