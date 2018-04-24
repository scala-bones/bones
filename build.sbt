lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.github.oletraveler",
      scalaVersion := "2.11.12", //TODO: cross compile 2.11.12 and 2.12.4
      version      := "0.3.0-SNAPSHOT"
    )),
    name := "Bones",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % "1.0.1",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "net.liftweb" %% "lift-json" % "2.6.3" % "optional", //Used for JsonExtract
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
      "org.scalatest" %% "scalatest" % "3.0.4" % Test,
      "org.easymock" % "easymock" % "3.5.1" % Test
    ),
    homepage := Some(url("https://github.com/oletraveler/bones")),
    startYear := Some(2018),
    description := "DSL for Data Description using ASTs and iterpreters",
    licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
    pomExtra := {
      <scm>
        <url>git://github.com/oletraveler/bones.git</url>
        <connection>scm:git://github.com/oletraveler/bones.git</connection>
      </scm>
      <developers>
        <developer>
          <id>oletraveler</id>
          <name>Travis Stevens</name>
          <url>https://github.com/oletraveler</url>
        </developer>
      </developers>
    }
  )

resolvers += Resolver.sonatypeRepo("releases")

testOptions in Test += Tests.Argument("-oF")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

credentials += Credentials(Path.userHome / ".ivy2" / ".credentials")

publishTo := {
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value)
    Some("snapshots" at nexus + "content/repositories/snapshots") 
  else
    Some("releases"  at nexus + "service/local/staging/deploy/maven2")
}

publishMavenStyle := true


