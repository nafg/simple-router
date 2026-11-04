import _root_.io.github.nafg.mergify.dsl.*
import _root_.io.github.nafg.scalacoptions.*

import sbtcrossproject.CrossPlugin.autoImport.{CrossType, crossProject}

ThisBuild / organization       := "io.github.nafg.simple-router"
ThisBuild / crossScalaVersions := Seq("2.13.18", "3.9.0")
ThisBuild / scalaVersion       := (ThisBuild / crossScalaVersions).value.last
// A bare setting, which sbt 2 applies to every project, so that each one evaluates it against its own Scala version
scalacOptions ++=
  ScalacOptions.all(scalaVersion.value)(
    (opts: options.Common) => opts.deprecation ++ opts.feature,
    (opts: options.V2_13) =>
      opts.Xsource("3") ++ opts.unchecked ++ List("-P:kind-projector:underscore-placeholders"),
    (opts: options.V3_8) => opts.XkindProjector("underscores")
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
      Compile / sourceGenerators += Def.task(PathAritySyntax.generate((Compile / sourceManaged).value)).taskValue,
      libraryDependencies ++=
        Seq(
          "org.scalatest"     %% "scalatest"       % "3.2.20"   % Test,
          "org.scalatestplus" %% "scalacheck-1-19" % "3.2.20.0" % Test
        ) ++
          PartialFunction.condOpt(CrossVersion.partialVersion(scalaVersion.value)) {
            case Some((2, _)) => compilerPlugin(
                ("org.typelevel" % "kind-projector" % "0.13.4").cross(CrossVersion.full).platform(Platform.jvm)
              )
          }
    )
    .jsSettings(
      libraryDependencies ++= Seq(
        "io.github.cquiroz" %% "scala-java-time" % "2.9.0" % Test,
        "org.scala-js"      %% "scalajs-dom"     % "2.8.1" % Test
      )
    )
