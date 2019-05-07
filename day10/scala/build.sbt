enablePlugins(ScalaJSPlugin)

name := "advent10"
scalaVersion := "2.12.8"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6"
libraryDependencies += "org.querki" %%% "jquery-facade" % "1.2"
