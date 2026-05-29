val scala3Version = "3.8.2"
val circeVersion = "0.14.15"
val circeOpticsVersion = "0.15.1"
val circeYamlVersion = "0.16.1"

lazy val root = project
  .in(file("."))
  .enablePlugins(ScalaNativePlugin) // Disable for development
  .settings(
    name := "OpenAPI Patcher",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "io.circe" %%% "circe-core" % circeVersion,
      "io.circe" %%% "circe-generic" % circeVersion,
      "io.circe" %%% "circe-parser" % circeVersion,
      "io.circe" %%% "circe-optics" % circeOpticsVersion,
      "io.circe" %%% "circe-yaml-scalayaml" % circeYamlVersion,
      "org.scalameta" %% "munit" % "1.0.0" % Test,
    ),
  )
