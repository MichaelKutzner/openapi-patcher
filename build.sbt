val scala3Version = "3.8.2"
val circeVersion = "0.14.15"
val circeOpticsVersion = "0.15.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "OpenAPI Patcher",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-generic" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "io.circe" %% "circe-optics" % circeOpticsVersion,
      "org.scalameta" %% "munit" % "1.0.0" % Test,
    ),
  )
