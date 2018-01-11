import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.gaia",
      scalaVersion := "2.11.12", //TODO: cross compile 2.12.4
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Soy",
    libraryDependencies ++= Seq(
      cats,
      "com.chuusai" %% "shapeless" % "2.3.3",
      "net.liftweb" %% "lift-json" % "2.6.3",
      scalaTest % Test
    )
  )

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")
