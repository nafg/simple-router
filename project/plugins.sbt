addSbtPlugin("org.scala-js"                             % "sbt-scalajs"              % "1.6.0")
addSbtPlugin("org.portable-scala"                       % "sbt-scalajs-crossproject" % "1.1.0")
addSbtPlugin("com.geirsson"                             % "sbt-ci-release"           % "1.5.7")
addSbtPlugin("com.codecommit"                           % "sbt-github-actions"       % "0.12.0")
libraryDependencies += "io.github.nafg.mergify"        %% "mergify-writer"           % "0.2.1"
libraryDependencies += "io.github.nafg.scalac-options" %% "scalac-options"           % "0.1.7"
