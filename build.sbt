lazy val commonSettings = Seq(
  organization := "com.github.oletraveler",
  scalaVersion := "2.12.7", //TODO: cross compile 2.11 and 2.12
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
  publishMavenStyle := true,
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

)
lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    name := "Bones",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "1.6.0",
      "org.typelevel" %% "cats-free" % "1.6.0",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
      //      "org.easymock" % "easymock" % "3.5.1" % Test
    ),
    description := "DSL for Data Description using ASTs and interpreters"
  )
lazy val testSchemas = (project in file("examples/test-schemas"))
  .settings(
    commonSettings,
    name := "Test Schemas",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  ).dependsOn(core)
lazy val scalacheck = (project in file("test-interpreters/scalacheck"))
  .settings(
    commonSettings,
    name := "Scalacheck",
    resolvers += "wolfendale" at "https://dl.bintray.com/wolfendale/maven/",
    libraryDependencies ++= Seq(
      //      "org.typelevel" %% "cats-core" % "1.6.0",
      //      "org.typelevel" %% "cats-free" % "1.6.0",
      //      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalacheck" %% "scalacheck" % "1.14.0",
      "wolfendale" %% "scalacheck-gen-regexp" % "0.1.1",
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
      //      "org.easymock" % "easymock" % "3.5.1" % Test
    ),
    description := "Interpreter to generate scalacheck proper generators"
  ).dependsOn(core, testSchemas % "test")
lazy val jsonOas3 = (project in file("interchange-format-interpreters/lift-json-oas3"))
  .settings(
    commonSettings,
    name := "DataDefinition to OAS3 Interpreter",
    libraryDependencies ++= Seq(
      "io.swagger.core.v3" % "swagger-core" % "2.0.7",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
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
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  ).dependsOn(core, jsonLift)
lazy val jsonLift = (project in file("interchange-format-interpreters/lift-json"))
  .settings(
    commonSettings,
    name := "Bones Json Lift",
    libraryDependencies ++= Seq (
      "net.liftweb" %% "lift-json" % "3.3.0",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  )
  .dependsOn(core)
lazy val stringJson = (project in file("interchange-format-interpreters/string-json"))
  .settings(
    commonSettings,
    name := "Bones String Json",
    libraryDependencies ++= Seq (
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  )
  .dependsOn(core, testSchemas % "test")
lazy val circeVersion = "0.11.1"
lazy val jsonCirce = (project in file("interchange-format-interpreters/circe"))
  .settings(
    commonSettings,
    name := "Bones Circe",
    libraryDependencies ++= Seq (
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  ).dependsOn(core)
lazy val jsonArgonaut = (project in file("interchange-format-interpreters/argonaut"))
  .settings(
    commonSettings,
    name := "Bones Argonaut",
    libraryDependencies ++= Seq (
      "io.argonaut" %% "argonaut" % "6.2.3",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  ).dependsOn(core)
lazy val protobuf = (project in file("interchange-format-interpreters/protobuf"))
  .settings(
    commonSettings,
    name := "Bones Protobuf",
    libraryDependencies ++= Seq (
      "com.google.protobuf" % "protobuf-java" % "3.7.1",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  ).dependsOn(core, testSchemas % "test->compile", scalacheck % "test->compile")
lazy val bson = (project in file("interchange-format-interpreters/bson"))
  .settings(
    commonSettings,
    name := "Bones Bson",
    libraryDependencies ++= Seq (
      "org.reactivemongo" %% "reactivemongo-bson" % "0.16.5"
    )
  ).dependsOn(core)
lazy val dbJdbc = (project in file("db-interpreters/jdbc"))
  .settings(
    commonSettings,
    name := "Bones JDBC",
    libraryDependencies ++= Seq(
      "org.postgresql" % "postgresql" % "42.2.5",
      "co.fs2" %% "fs2-core" % "1.0.4",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  ).dependsOn(core, testSchemas % "test->compile")
lazy val http4sVersion = "0.20.0"
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
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  ).dependsOn(core, jsonCirce, jsonOas3, protobuf, bson)
lazy val react = (project in file("client-interpreters/react"))
  .settings(
    commonSettings,
    name := "Bones React",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "scalatags" % "0.6.7",
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  ).dependsOn(core, testSchemas % "test")
lazy val http4sClient = (project in file("client-interpreters/http4s-client"))
  .settings(
    commonSettings,
    name := "Bones React",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-client" % http4sVersion,
      "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
      "org.scalatest" %% "scalatest" % "3.0.7" % Test
    )
  ).dependsOn(core, jsonCirce, testSchemas % "test")
lazy val examples = (project in file("examples/http4s-examples"))
    .settings(
      commonSettings,
      name := "Bones Examples",
      libraryDependencies ++= Seq(
        "io.swagger.core.v3" % "swagger-jaxrs2" % "2.0.5",
        "ws.unfiltered" %% "unfiltered-jetty" % "0.9.1",
        "io.swagger" % "swagger-parser" % "1.0.44",
        "org.slf4j" % "slf4j-simple" % "1.7.26",
        "com.zaxxer" % "HikariCP" % "3.3.1",
        "org.scalacheck" %% "scalacheck" % "1.14.0" % Test,
        "org.scalatest" %% "scalatest" % "3.0.7" % Test
//        "org.easymock" % "easymock" % "3.5.1" % Test
      )
    ).dependsOn(core, jsonOas3, dbJdbc, restHttp4s, jsonOas3, protobuf, react, testSchemas % "test")



//enablePlugins(TutPlugin)

testOptions in Test += Tests.Argument("-oF")

resolvers += "tPoleCat" at "https://dl.bintray.com/tpolecat/maven/"
resolvers += "wolfendale" at "https://dl.bintray.com/wolfendale/maven/"


