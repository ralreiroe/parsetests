import Dependencies._
import sbt.Keys.libraryDependencies

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.2",
      version      := "0.1.0-SNAPSHOT"
    )),

    resolvers += "bintray-djspiewak-maven" at "https://dl.bintray.com/djspiewak/maven",

    name := "Parsetests",
    libraryDependencies += scalaTest % Test,
    libraryDependencies += "com.typesafe.play" % "play-json_2.12" % "2.6.2",
      libraryDependencies += "com.codecommit" %% "parseback-core" % "0.3",
    libraryDependencies += "com.codecommit" %% "parseback-cats" % "0.3",
    libraryDependencies += "org.scalacheck" % "scalacheck_2.12" % "1.13.5" % "test",
    libraryDependencies += "org.julienrf" % "play-json-derived-codecs_2.12" % "4.0.0",
    libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.4"
)
