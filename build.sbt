import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}


ThisBuild / organization := "io.github.nafg.simple-router"
ThisBuild / crossScalaVersions := Seq("2.12.12", "2.13.4")
ThisBuild / scalaVersion := (ThisBuild / crossScalaVersions).value.last
ThisBuild / scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature")

lazy val core =
  crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full)
    .in(file("."))
    .settings(
      name := "core",
      developers :=
        List(Developer("nafg", "Naftoli Gugenheim", "nafg@users.noreply.github.com", url("https://github.com/nafg"))),
      homepage := Some(url("https://github.com/nafg/simple-router")),
      licenses := Seq(("Apache 2", url("https://www.apache.org/licenses/LICENSE-2.0.txt"))),
      scmInfo := Some(
        ScmInfo(
          browseUrl = url("https://github.com/nafg/simple-router"),
          connection = "scm:git:git://github.com/nafg/simple-router.git",
          devConnection = Some("scm:git:git@github.com:nafg/simple-router.git")
        )
      ),
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "3.2.3" % Test,
        "org.scalatestplus" %% "scalacheck-1-15" % "3.2.3.0" % Test
      )
    )
