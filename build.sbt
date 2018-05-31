lazy val commonSettings = Seq(
  organization := "com.github.oletraveler",
  scalaVersion := "2.11.12", //TODO: cross compile 2.11.12 and 2.12.4
  version      := "0.4.0-SNAPSHOT",
  homepage := Some(url("https://github.com/oletraveler/bones")),
  startYear := Some(2018),
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
  },
  resolvers += Resolver.sonatypeRepo("releases"),
  credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots") 
    else
      Some("releases"  at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true


)
lazy val restUnfiltered = (project in file("rest-interpreters/unfiltered"))
  .settings(
    commonSettings,
    name := "Bones Rest Unfiltered",
    libraryDependencies ++= Seq(
      "net.databinder" %% "unfiltered-filter" % "0.8.4",
      "javax.servlet" % "servlet-api" % "2.5",
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
      "org.scalatest" %% "scalatest" % "3.0.4" % Test,
      "org.easymock" % "easymock" % "3.5.1" % Test
    )
  ).dependsOn(core)
lazy val jsonLift = (project in file("json-interpreters/lift-json"))
  .settings(
    commonSettings,
    name := "Bones Json Lift",
    libraryDependencies ++= Seq (
      "net.liftweb" %% "lift-json" % "2.6.3",
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
      "org.scalatest" %% "scalatest" % "3.0.4" % Test,
      "org.easymock" % "easymock" % "3.5.1" % Test
    )
  )
  .dependsOn(core)
lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    name := "Bones",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-free" % "1.0.1",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
      "org.scalatest" %% "scalatest" % "3.0.4" % Test,
      "org.easymock" % "easymock" % "3.5.1" % Test
    ),
    description := "DSL for Data Description using ASTs and iterpreters"
  )


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

testOptions in Test += Tests.Argument("-oF")

