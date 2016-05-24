name := "Exam Adpro"

val libraryVersion = "1.2.0-M1" // or "1.3.0-SNAPSHOT"

version := "0.0"

scalaVersion := "2.11.7"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.5" % "compile"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "compile"

libraryDependencies += "com.lihaoyi" %% "scalaparse" % "0.3.1" % "compile"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"    % libraryVersion % "compile",
  "com.github.julien-truffaut"  %%  "monocle-generic" % libraryVersion % "compile",
  "com.github.julien-truffaut"  %%  "monocle-macro"   % libraryVersion % "compile",
  "com.github.julien-truffaut"  %%  "monocle-state"   % libraryVersion % "compile",
  "com.github.julien-truffaut"  %%  "monocle-law"     % libraryVersion % "compile"
)

val scalazVersion = "7.1.3"

libraryDependencies ++= Seq (
  "org.scalaz" %% "scalaz-core"               % scalazVersion % "compile",
  "org.scalaz" %% "scalaz-scalacheck-binding" % scalazVersion % "compile" )

// addCompilerPlugin("org.scalamacros" %% "paradise" % "2.0.1" cross CrossVersion.full)