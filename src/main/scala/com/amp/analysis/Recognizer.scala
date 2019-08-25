package com.amp.analysis

import scala.reflect.ClassTag

object Recognizer {
  def recognize[T](x: Any)(implicit tag: ClassTag[T]): Boolean =
    x match {
      case _: T => true
      case _ => false
    }
  def recognize[T](seq: Seq[Any])(implicit tag: ClassTag[T]): Boolean = {
    seq.forall(x => recognize(x))
  }
}