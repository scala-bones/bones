lazy val commonSettings = Seq(
  organization := "com.github.oletraveler",
  scalaVersion := "2.12.7", //TODO: cross compile 2.11.12 and 2.12.4
  version      := "0.5.0-SNAPSHOT",
  homepage := Some(url("https://github.com/oletraveler/bones")),
  startYear := Some(2018),
  licenses := Seq("MIT" -> url("http://opensource.org/licenses/MIT")),
  scalacOptions ++= Seq("-Ypartial-unification"),
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
lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    name := "Bones",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.4.0",
      "org.typelevel" %% "cats-free" % "1.4.0",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
      //      "org.easymock" % "easymock" % "3.5.1" % Test
    ),
    description := "DSL for Data Description using ASTs and interpreters"
  )
lazy val jsonOas3 = (project in file("json-interpreters/lift-json-oas3"))
  .settings(
    commonSettings,
    name := "DataDefinition to OAS3 Interpreter",
    libraryDependencies ++= Seq(
      "io.swagger.core.v3" % "swagger-core" % "2.0.5",
      "io.argonaut" %% "argonaut" % "6.2.2",
      "io.argonaut" %% "argonaut-monocle" % "6.2.2",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
//      "org.easymock" % "easymock" % "3.5.1" % Test
    )
  ).dependsOn(core)
lazy val doobieVersion = "0.6.0"
lazy val restUnfiltered = (project in file("rest-interpreters/unfiltered"))
  .settings(
    commonSettings,
    name := "Bones Rest Unfiltered",
    libraryDependencies ++= Seq(
      "javax.servlet" % "javax.servlet-api" % "3.0.1",
      "ws.unfiltered" %% "unfiltered-filter" % "0.9.1",
      "org.tpolecat" %% "doobie-core" % doobieVersion,
      "org.tpolecat" %% "doobie-postgres" % doobieVersion,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  ).dependsOn(core, jsonLift)
lazy val jsonLift = (project in file("json-interpreters/lift-json"))
  .settings(
    commonSettings,
    name := "Bones Json Lift",
    libraryDependencies ++= Seq (
      "net.liftweb" %% "lift-json" % "3.3.0",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  )
  .dependsOn(core)
lazy val circeVersion = "0.10.1"
lazy val jsonCirce = (project in file("json-interpreters/circe"))
  .settings(
    commonSettings,
    name := "Bones Circe",
    libraryDependencies ++= Seq (
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  ).dependsOn(core)
lazy val jsonArgonaut = (project in file("json-interpreters/argonaut"))
  .settings(
    commonSettings,
    name := "Bones Argonaut",
    libraryDependencies ++= Seq (
      "io.argonaut" %% "argonaut" % "6.2.2",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  ).dependsOn(core)
lazy val dbDoobie = (project in file("db-interpreters/doobie"))
  .settings(
    commonSettings,
    name := "Bones Doobie",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core" % doobieVersion,
      "org.tpolecat" %% "doobie-postgres" % doobieVersion
    )
  ).dependsOn(core)
lazy val http4sVersion = "0.20.0-M3"
lazy val restHttp4s = (project in file("rest-interpreters/http4s-interpreter"))
  .settings(
    commonSettings,
    name := "Bones Http4s Circe",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
//      "org.http4s" %% "http4s-blaze-client" % http4sVersion,
//      "io.swagger.core.v3" % "swagger-core" % "2.0.5",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.5" % Test
    )
  ).dependsOn(core, jsonCirce, jsonOas3)
lazy val examples = (project in file("examples"))
    .settings(
      commonSettings,
      name := "Bones Examples",
      libraryDependencies ++= Seq(
        "io.swagger.core.v3" % "swagger-jaxrs2" % "2.0.5",
        "ws.unfiltered" %% "unfiltered-jetty" % "0.9.1",
        "org.tpolecat" %% "doobie-hikari" % doobieVersion,
        "io.swagger" % "swagger-parser" % "1.0.36",
        "org.slf4j" % "slf4j-simple" % "1.6.3",
        "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
        "org.scalatest" %% "scalatest" % "3.0.5" % Test
//        "org.easymock" % "easymock" % "3.5.1" % Test
      )
    ).dependsOn(core, jsonOas3, dbDoobie, restHttp4s, jsonOas3)


addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

//enablePlugins(TutPlugin)

testOptions in Test += Tests.Argument("-oF")

