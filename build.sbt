name := "cats-workshop"

version := "0.1"

scalaVersion := "2.13.8"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.7.0"

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.2" cross CrossVersion.full)
