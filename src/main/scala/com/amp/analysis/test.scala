package com.amp.analysis
import spoon.Launcher
import spoon.reflect.CtModel

object test{
  def main(args: Array[String]): Unit = {

    val launcher = new Launcher

    // path can be a folder or a file
    // addInputResource can be called several times
    launcher.addInputResource("<path_to_source>")

    launcher.buildModel

    val model = launcher.getModel
  }
}
