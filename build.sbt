name := "AMP"

version := "0.1"

scalaVersion := "2.13.0"

libraryDependencies += "fr.inria.gforge.spoon" % "spoon-core" % "7.5.0" exclude("org.eclipse.platform", "org.eclipse.equinox.app")
libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.8" % Test

enablePlugins(PackPlugin)
