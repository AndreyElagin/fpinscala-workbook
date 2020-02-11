ThisBuild / name := "fpinscala"
ThisBuild / scalaVersion := "2.13.1"
ThisBuild / version := "0.1"

ThisBuild / scalacOptions ++= Seq(
  "-deprecation",
  "-explaintypes",
  "-feature",
  "-language:higherKinds",
  "-unchecked",
  "-Xcheckinit",
  "-Xlint:adapted-args",
  "-Xlint:infer-any",
  "-Xlint:nullary-override",
  "-Xlint:nullary-unit",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

scalaVersion := "2.13.1"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-effect" % "2.1.0",
  "co.fs2" %% "fs2-core" % "2.2.1",
  "co.fs2" %% "fs2-io" % "2.2.1",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)
