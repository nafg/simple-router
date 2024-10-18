import _root_.io.github.nafg.mergify.dsl.*
import _root_.io.github.nafg.scalacoptions.*

import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

ThisBuild / organization       := "io.github.nafg.simple-router"
ThisBuild / crossScalaVersions := Seq("2.13.15", "3.6.0")
ThisBuild / scalaVersion       := (ThisBuild / crossScalaVersions).value.last
ThisBuild / scalacOptions ++=
  ScalacOptions.all(scalaVersion.value)(
    (opts: options.Common) => opts.deprecation ++ opts.unchecked ++ opts.feature,
    (opts: options.V2_13) => opts.Xsource("3"),
    (opts: options.V3) => opts.YkindProjector
  )

mergifyExtraConditions := Seq(
  (Attr.Author :== "scala-steward") ||
    (Attr.Author :== "nafg-scala-steward[bot]")
)

lazy val core =
  crossProject(JVMPlatform, JSPlatform).crossType(CrossType.Full)
    .in(file("."))
    .settings(
      name := "core",
      libraryDependencies ++=
        Seq(
          "org.scalatest"     %% "scalatest"       % "3.2.19"   % Test,
          "org.scalatestplus" %% "scalacheck-1-16" % "3.2.14.0" % Test
        ) ++
          PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
            case Some((2, _)) => compilerPlugin("org.typelevel" % "kind-projector" % "0.13.3" cross CrossVersion.full)
          }
    )
