//import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

//workbenchSettings
// this configuration is only for initial develop of the Cassowary Solver

name := "CyberthinkersProblemSolvers"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.8.2",
  "com.lihaoyi" %%% "scalatags" % "0.5.4"
)

//bootSnippet := "example.ScalaJSExample().main(document.getElementById('canvas'));"

//updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)