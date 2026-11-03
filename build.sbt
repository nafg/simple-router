import _root_.io.github.nafg.mergify.dsl.*
import _root_.io.github.nafg.scalacoptions.*

import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

ThisBuild / organization       := "io.github.nafg.simple-router"
ThisBuild / crossScalaVersions := Seq("2.13.18", "3.9.0")
ThisBuild / scalaVersion       := (ThisBuild / crossScalaVersions).value.last
// Bare settings, which sbt 2 applies to every project, so that each one evaluates them against its own Scala version
scalacOptions ++=
  ScalacOptions.all(scalaVersion.value)(
    (opts: options.Common) => opts.deprecation ++ opts.feature,
    (opts: options.V2_13) =>
      opts.Xsource("3") ++ opts.unchecked ++ List("-P:kind-projector:underscore-placeholders"),
    (opts: options.V3_8) => opts.XkindProjector("underscores")
  )
libraryDependencies ++=
  PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
    case Some((2, _)) => compilerPlugin(
        ("org.typelevel" % "kind-projector" % "0.13.4").cross(CrossVersion.full).platform(Platform.jvm)
      )
  }

mergifyExtraConditions := Seq(
  (Attr.Author :== "scala-steward") ||
    (Attr.Author :== "nafg-scala-steward[bot]")
)

// Stringable's instances refer to java.time, which Scala.js provides only with this library
lazy val jsJavaTimeForTests = libraryDependencies += "io.github.cquiroz" %% "scala-java-time" % "2.8.0" % Test

lazy val core =
  crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full)
    .in(file("."))
    .settings(
      name := "core",
      libraryDependencies ++=
        Seq(
          "org.scalatest"     %% "scalatest"       % "3.2.20"   % Test,
          "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test
        )
    )
    .jsSettings(
      jsJavaTimeForTests,
      libraryDependencies += "org.scala-js" %% "scalajs-dom" % "2.8.1" % Test
    )

lazy val laws =
  crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full)
    .in(file("laws"))
    .dependsOn(core)
    .settings(
      name                                    := "laws",
      libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.20.0"
    )
    .jsSettings(jsJavaTimeForTests)

lazy val derivation =
  crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Pure)
    .in(file("derivation"))
    .dependsOn(core, laws % Test)
    .settings(
      name := "derivation",
      libraryDependencies ++=
        (CrossVersion.partialVersion(scalaVersion.value) match {
          case Some((2, _)) =>
            Seq(
              "com.softwaremill.magnolia1_2" %% "magnolia"      % "1.1.14",
              "org.scala-lang"                % "scala-reflect" % scalaVersion.value % Provided
            )
          case _            => Seq("com.softwaremill.magnolia1_3" %% "magnolia" % "1.3.24")
        })
    )
    .jsSettings(jsJavaTimeForTests)
