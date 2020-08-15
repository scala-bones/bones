lazy val commonSettings = Seq(
  organization := "com.github.oletraveler",
  scalaVersion := "2.13.2",
  version := "0.7.0-SNAPSHOT",
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
  resolvers += Resolver.sonatypeRepo("snapshots"),
  credentials += Credentials(Path.userHome / ".ivy2" / ".credentials"),
  publishTo := {
    val nexus = "https://oss.sonatype.org/"
    if (isSnapshot.value)
      Some("snapshots" at nexus + "content/repositories/snapshots")
    else
      Some("releases" at nexus + "service/local/staging/deploy/maven2")
  },
  publishMavenStyle := true,
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.0" cross CrossVersion.full),
  scalacOptions ++= Seq(
    "-encoding", "utf8", // Option and arguments on same line
    "-Xfatal-warnings",  // New lines for each options
    "-deprecation",
    "-unchecked",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps"
  )
)
lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    name := "Bones",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.1.1",
      "org.typelevel" %% "cats-free" % "2.1.1",
      "com.chuusai" %% "shapeless" % "2.3.3",
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
      "org.scalatest" %% "scalatest-mustmatchers" % "3.2.0" % Test
    ),
    description := "DSL for Data Description using ASTs and interpreters"
  )
lazy val testSchemas = (project in file("examples/test-schemas"))
  .settings(
    commonSettings,
    name := "Bones Test Schemas",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core)
lazy val scalacheck = (project in file("test-interpreters/scalacheck"))
  .settings(
    commonSettings,
    name := "Bones Scalacheck",
    resolvers += "wolfendale" at "https://dl.bintray.com/wolfendale/maven/",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.3",
      "wolfendale" %% "scalacheck-gen-regexp" % "0.1.2",
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
      //      "org.easymock" % "easymock" % "3.5.1" % Test
    ),
    description := "Interpreter to generate scalacheck proper generators"
  )
  .dependsOn(core, testSchemas % "test")
lazy val swaggerOas3 = (project in file("interchange-format-interpreters/swagger-oas3"))
  .settings(
    commonSettings,
    name := "Bones DataDefinition to OAS3 Interpreter",
    libraryDependencies ++= Seq(
      "io.swagger.core.v3" % "swagger-core" % "2.1.4",
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test")
lazy val doobieVersion = "0.6.0"
// lazy val directEncoders = (project in file("interchange-format-interpreters/direct-encoders"))
//  .settings(
//    commonSettings,
//    name := "Bones String Json",
//    libraryDependencies ++= Seq(
//      "org.apache.commons" % "commons-text" % "1.9",
//      "io.circe" %% "circe-core" % circeVersion % Test,
//      "io.circe" %% "circe-parser" % circeVersion % Test,
//      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
//      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
//      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
//    )
//  )
//  .dependsOn(core, testSchemas % "test", scalacheck % "test")
lazy val circeVersion = "0.13.0"
lazy val jsonCirce = (project in file("interchange-format-interpreters/circe"))
  .settings(
    commonSettings,
    name := "Bones Circe",
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % circeVersion,
      "io.circe" %% "circe-parser" % circeVersion,
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test", scalacheck % "test")
lazy val jsonArgonaut = (project in file("interchange-format-interpreters/argonaut"))
  .settings(
    commonSettings,
    name := "Bones Argonaut",
    libraryDependencies ++= Seq(
      "io.argonaut" %% "argonaut" % "6.3.0",
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test", scalacheck % "test")
lazy val protobuf = (project in file("interchange-format-interpreters/protobuf"))
  .settings(
    commonSettings,
    name := "Bones Protobuf",
    libraryDependencies ++= Seq(
      "com.google.protobuf" % "protobuf-java" % "3.13.0",
      "com.google.protobuf" % "protobuf-java-util" % "3.13.0",
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test->compile", scalacheck % "test->compile")
lazy val bson = (project in file("interchange-format-interpreters/bson"))
  .settings(
    commonSettings,
    name := "Bones Bson",
    libraryDependencies ++= Seq(
      "org.reactivemongo" %% "reactivemongo-bson" % "0.20.12",
      "org.reactivemongo" %% "reactivemongo" % "0.20.12",
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test", scalacheck % "test")
lazy val dbJdbc = (project in file("db-interpreters/jdbc"))
  .settings(
    commonSettings,
    name := "Bones JDBC",
    libraryDependencies ++= Seq(
      "org.postgresql" % "postgresql" % "42.2.14",
      "co.fs2" %% "fs2-core" % "2.4.2",
      "io.github.scala-bones" %% "scatonic-ideal" % "0.2.0-SNAPSHOT",
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test->compile")

lazy val http4sVersion = "0.21.7"
lazy val restHttp4s = (project in file("rest-interpreters/http4s-interpreter"))
  .settings(
    commonSettings,
    name := "Bones Http4s Server",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, jsonCirce, swaggerOas3, protobuf, bson)
lazy val awsLambda = (project in file("rest-interpreters/aws-lambda"))
  .settings(
    commonSettings,
    name := "Bones AWS Lambda Server",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, jsonCirce, swaggerOas3, protobuf, bson)
lazy val http4sClient = (project in file("client-interpreters/http4s-client"))
  .settings(
    commonSettings,
    name := "Bones Http4s Client",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-client" % http4sVersion,
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, jsonCirce, testSchemas % "test")
lazy val examples = (project in file("examples/http4s-examples"))
  .settings(
    commonSettings,
    name := "Bones Examples",
    libraryDependencies ++= Seq(
      "io.swagger.core.v3" % "swagger-jaxrs2" % "2.1.4",
      "io.swagger" % "swagger-parser" % "1.0.51",
      "org.slf4j" % "slf4j-simple" % "1.7.30",
      "com.zaxxer" % "HikariCP" % "3.4.5",
      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.0" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(
    core,
    swaggerOas3,
    dbJdbc,
    restHttp4s,
    swaggerOas3,
    protobuf,
    jsonArgonaut % "test",
    bson % "test",
//    directEncoders % "test",
    testSchemas % "test",
    scalacheck % "test"
  )
lazy val protobufIntegrationTest =
  (project in file("interchange-format-interpreters/javapb-integration-test"))
    .settings(
      commonSettings,
      name := "Bones JavaPB Integration Test",
      libraryDependencies ++= Seq(
        "com.google.protobuf" % "protobuf-java" % "3.13.0",
        "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
        "org.scalatest" %% "scalatest" % "3.2.0" % Test,
        "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
      )
    )
    .dependsOn(protobuf % "test", testSchemas % "test")

//enablePlugins(TutPlugin)

testOptions in Test += Tests.Argument("-oF")

resolvers += "tPoleCat" at "https://dl.bintray.com/tpolecat/maven/"
resolvers += "wolfendale" at "https://dl.bintray.com/wolfendale/maven/"
