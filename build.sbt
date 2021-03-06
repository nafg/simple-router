import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}


ThisBuild / organization := "io.github.nafg.simple-router"
ThisBuild / crossScalaVersions := Seq("2.12.14", "2.13.6")
ThisBuild / scalaVersion := (ThisBuild / crossScalaVersions).value.last
ThisBuild / scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

lazy val core =
  crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full)
    .in(file("."))
    .settings(
      name := "core",
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.2.9" % Test,
        "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" % Test
      )
    )
