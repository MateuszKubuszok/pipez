// linters
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")
// versioning
addSbtPlugin("com.github.sbt" % "sbt-git" % "2.0.0")
// cross-compile
addSbtPlugin("com.eed3si9n" % "sbt-projectmatrix" % "0.9.0")
addSbtPlugin("com.indoorvivants" % "sbt-commandmatrix" % "0.0.5")
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.12.0")
addSbtPlugin("org.scala-native" % "sbt-scala-native" % "0.4.9")
// disabling projects in IDEs
addSbtPlugin("org.jetbrains" % "sbt-ide-settings" % "1.1.0")
// welcome
addSbtPlugin("com.github.reibitto" % "sbt-welcome" % "0.2.2")

ThisBuild / libraryDependencySchemes += "org.scala-lang.modules" %% "scala-xml" % VersionScheme.Always
