enablePlugins(ScalaJSPlugin)
enablePlugins(ScalaJSBundlerPlugin)
//enablePlugins(WebScalaJSBundlerPlugin)

name := "Frondend 0.1.3"

version := "0.1.1"

scalaVersion := "2.12.3"

scalaJSUseMainModuleInitializer := true

libraryDependencies ++= Seq(
  "com.github.japgolly.scalajs-react" %%% "core" % "1.1.0",
  "com.github.japgolly.scalajs-react" %%% "extra" % "1.1.0",
  "com.olvind" %%% "scalajs-react-components" % "0.8.0",
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "org.scalatest" %%% "scalatest" % "3.0.0" % Test
)

npmDependencies in Compile ++= Seq(
  "react" -> "15.6.1",
  "react-dom" -> "15.6.1",
  "snabbdom" -> "0.5.3"
)
npmDevDependencies in Compile += "uglifyjs-webpack-plugin" -> "0.4.3"



webpackConfigFile in fullOptJS := Some(baseDirectory.value / "prod.webpack.config.js")

webpackMonitoredDirectories += baseDirectory.value

// Execute the tests in browser-like environment
requiresDOM in Test := true
webpackBundlingMode := BundlingMode.LibraryAndApplication()
//useYarn := true


// Check that a HTML can be loaded (and that its JavaScript can be executed) without errors
InputKey[Unit]("html") := {
  import complete.DefaultParsers._
  val page = (Space ~> StringBasic).parsed
  import com.gargoylesoftware.htmlunit.WebClient
  val client = new WebClient()
  try {
    client.getPage(s"file://${baseDirectory.value.absolutePath}/$page")
  } finally {
    client.close()
  }
}

TaskKey[Unit]("checkSize") := {
  //#filter-files
  val files = (webpack in (Compile, fullOptJS)).value
  val bundleFile = files
    .find(_.metadata.get(BundlerFileTypeAttr).exists(_ == BundlerFileType.ApplicationBundle))
    .get.data
  //#filter-files
  val artifactSize = IO.readBytes(bundleFile).length

  val sizeLow = 20000
  val sizeHigh = 22000

  // Account for minor variance in size due to transitive dependency updates
  assert(
    artifactSize >= sizeLow && artifactSize <= sizeHigh,
    s"expected: [$sizeLow, $sizeHigh], got: $artifactSize"
  )
}