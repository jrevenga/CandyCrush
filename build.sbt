ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .settings(
    name := "PL3"
  )


unmanagedResourceDirectories in Compile += baseDirectory.value / "src" / "main" / "resources"

//Amazon
libraryDependencies += "com.amazonaws" % "aws-java-sdk" % "1.12.429"
libraryDependencies += "mysql" % "mysql-connector-java" % "8.0.32"
libraryDependencies += "com.typesafe.play" %% "play-json" % "2.9.4"

//Google
libraryDependencies += "com.google.cloud" % "google-cloud-datastore" % "2.3.0"
libraryDependencies ++= Seq(
  "com.google.cloud" % "google-cloud-datastore" % "2.14.0",
  "org.json4s" %% "json4s-native" % "4.0.6"
)

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"

unmanagedResourceDirectories in Compile += baseDirectory.value / "resources"
