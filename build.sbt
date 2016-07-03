import com.lihaoyi.workbench.Plugin._

enablePlugins(ScalaJSPlugin)

workbenchSettings

name := "DiffSolver"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
	"com.lihaoyi" %%% "scalatags" % "0.5.5",
	"org.scalatest" %%% "scalatest" % "3.0.0-M10" % "test"
)

bootSnippet := "new ch.ethz.ipes.buschr.schematics.CircuitAnalyzer(document.getElementById('canvas'));"

updateBrowsers <<= updateBrowsers.triggeredBy(fastOptJS in Compile)

excludeFilter in unmanagedSources := HiddenFileFilter || "excluded"

scalaJSUseRhino in Global := false
