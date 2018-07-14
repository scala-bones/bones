lazy val commonSettings = Seq(
  organization := "com.github.oletraveler",
  scalaVersion := "2.11.12", //TODO: cross compile 2.11.12 and 2.12.4
  version      := "0.5.0-SNAPSHOT",
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
lazy val jsonOas3 = (project in file("json-interpreters/lift-json-oas3"))
  .settings(
    commonSettings,
    name := "DataDefinition to OAS3 Interpreter",
    libraryDependencies ++= Seq(
      "io.argonaut" %% "argonaut" % "6.2",
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
      "org.scalatest" %% "scalatest" % "3.0.4" % Test,
      "org.easymock" % "easymock" % "3.5.1" % Test
    )
  ).dependsOn(core)
lazy val doobieVersion = "0.5.3"
lazy val restUnfiltered = (project in file("rest-interpreters/unfiltered"))
  .settings(
    commonSettings,
    name := "Bones Rest Unfiltered",
    libraryDependencies ++= Seq(
      "javax.servlet" % "javax.servlet-api" % "3.0.1",
      "net.databinder" %% "unfiltered-filter" % "0.8.4",
      "org.tpolecat" %% "doobie-core" % doobieVersion,
      "org.tpolecat" %% "doobie-postgres" % doobieVersion,
      "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
      "org.scalatest" %% "scalatest" % "3.0.4" % Test,
      "org.easymock" % "easymock" % "3.5.1" % Test
    )
  ).dependsOn(core, jsonLift)
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
lazy val examples = (project in file("examples"))
    .settings(
      commonSettings,
      name := "Bones Examples",
      libraryDependencies ++= Seq(
        "net.databinder" %% "unfiltered-jetty" % "0.8.4",
        "io.swagger" % "swagger-parser" % "1.0.36",
        "org.scalacheck" %% "scalacheck" % "1.13.4" % Test,
        "org.scalatest" %% "scalatest" % "3.0.4" % Test,
        "org.easymock" % "easymock" % "3.5.1" % Test
      )
    ).dependsOn(jsonLift, restUnfiltered, jsonOas3)


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.4")

//enablePlugins(TutPlugin)

testOptions in Test += Tests.Argument("-oF")

