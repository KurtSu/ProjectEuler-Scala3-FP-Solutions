ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "ProjectEuler-Scala3-FP-Solutions",
    idePackagePrefix := Some("net.projecteuler.kurtsu")
  )
