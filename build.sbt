enablePlugins(ScalaJSPlugin)

name := "InfectiousDiseaseSimulatorJS"
scalaVersion := "2.13.1"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true
mainClass := Some("sim.Main")

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "1.0.0"

//libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"


