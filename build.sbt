lazy val scala212 = "2.12.12"
lazy val scala213 = "2.13.3"
lazy val supportedScalaVersions = List(scala212, scala213)

def versionSpecificOptions(scalaVersion: String): Seq[String] = {
  CrossVersion.partialVersion(scalaVersion) match {
    case Some((2, scalaMajor)) if scalaMajor == 13 => Nil
    case _                                         => Seq("-Ypartial-unification")
  }
}

def scalacOptionsVersion(scalaVersion: String) = {
  Seq(
//    "-deprecation",
//    "-encoding", "UTF-8",
//    "-unchecked",
//    "-features",
//    "-Xlint",
//   "-Xfatal-warnings",
//    "-Ywarn-dead-code",
    "-language:implicitConversions",
    "-language:higherKinds",
    "-language:existentials",
    "-language:postfixOps"
  ) ++ versionSpecificOptions(scalaVersion)
}

lazy val commonSettings = Seq(
  organization := "io.github.scala-bones",
  scalaVersion := "2.13.2",
  crossScalaVersions := supportedScalaVersions,
  scalacOptions := scalacOptionsVersion(scalaVersion.value),
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
  addCompilerPlugin("org.typelevel" %% "kind-projector" % "0.11.3" cross CrossVersion.full)
)
lazy val core = (project in file("core"))
  .settings(
    commonSettings,
    name := "Bones",
    libraryDependencies ++= Seq(
      "com.chuusai" %% "shapeless" % "2.3.4",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
      "org.scalatest" %% "scalatest-mustmatchers" % "3.2.8" % Test
    ),
    description := "DSL for Data Description using ASTs and interpreters"
  )
lazy val coreCats = (project in file("core-cats"))
  .settings(
    commonSettings,
    name := "Bones Cats",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.6.0",
      "com.chuusai" %% "shapeless" % "2.3.4",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
      "org.scalatest" %% "scalatest-mustmatchers" % "3.2.8" % Test
    ),
    description := "DSL for Data Description using ASTs and interpreters"
  )
  .dependsOn(core)
lazy val testSchemas = (project in file("examples/test-schemas"))
  .settings(
    commonSettings,
    name := "Bones Test Schemas",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
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
      "org.scalacheck" %% "scalacheck" % "1.15.3",
      "wolfendale" %% "scalacheck-gen-regexp" % "0.1.2",
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
      //      "org.easymock" % "easymock" % "3.5.1" % Test
    ),
    description := "Interpreter to generate scalacheck proper generators"
  )
  .dependsOn(coreCats, testSchemas % "test")
lazy val swaggerOas3 = (project in file("interchange-format-interpreters/swagger-oas3"))
  .settings(
    commonSettings,
    name := "Bones DataDefinition to OAS3 Interpreter",
    libraryDependencies ++= Seq(
      "io.swagger.core.v3" % "swagger-core" % "2.1.9",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test")
val tapirVersion = "0.17.19"
lazy val tapirTransformation = (project in file("transformation/tapir"))
  .settings(
    commonSettings,
    name := "Bones Tapir Transformation",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-core" % tapirVersion,
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test")
lazy val tapirCirceSkeleton = (project in file("skeleton/tapir-circe"))
  .settings(
    commonSettings,
    name := "Bones Tapir Circe Codec",
    libraryDependencies ++= Seq(
      "com.softwaremill.sttp.tapir" %% "tapir-json-circe" % tapirVersion,
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(tapirTransformation, jsonCirce, testSchemas % "test")
lazy val doobieVersion = "1.0.0-M1"
// lazy val directEncoders = (project in file("interchange-format-interpreters/direct-encoders"))
//  .settings(
//    commonSettings,
//    name := "Bones String Json",
//    libraryDependencies ++= Seq(
//      "org.apache.commons" % "commons-text" % "1.9",
//      "io.circe" %% "circe-core" % circeVersion % Test,
//      "io.circe" %% "circe-parser" % circeVersion % Test,
//      "org.scalacheck" %% "scalacheck" % "1.14.3" % Test,
//      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
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
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test", scalacheck % "test")
lazy val jsonArgonaut = (project in file("interchange-format-interpreters/argonaut"))
  .settings(
    commonSettings,
    name := "Bones Argonaut",
    libraryDependencies ++= Seq(
      "io.argonaut" %% "argonaut" % "6.3.3",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test", scalacheck % "test")
lazy val jsonSpray = (project in file("interchange-format-interpreters/spray"))
  .settings(
    commonSettings,
    name := "Bones Spray Json",
    libraryDependencies ++= Seq(
      "io.spray" %% "spray-json" % "1.3.6",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test", scalacheck % "test")
lazy val json4s = (project in file("interchange-format-interpreters/json4s"))
  .settings(
    commonSettings,
    name := "Bones Json4s",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-core" % "3.6.11",
      "org.json4s" %% "json4s-native" % "3.6.11" % Test,
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test", scalacheck % "test")
lazy val protobuf = (project in file("interchange-format-interpreters/protobuf"))
  .settings(
    commonSettings,
    name := "Bones Protobuf",
    libraryDependencies ++= Seq(
      "com.google.protobuf" % "protobuf-java" % "3.24.4",
      "com.google.protobuf" % "protobuf-java-util" % "3.24.4",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test->compile", scalacheck % "test->compile")
lazy val bson = (project in file("interchange-format-interpreters/bson"))
  .settings(
    commonSettings,
    name := "Bones Bson",
    libraryDependencies ++= Seq(
      "org.reactivemongo" %% "reactivemongo-bson" % "0.20.13",
      "org.reactivemongo" %% "reactivemongo" % "1.0.3",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test", scalacheck % "test")
lazy val dbJdbc = (project in file("db-interpreters/jdbc"))
  .settings(
    commonSettings,
    name := "Bones JDBC",
    libraryDependencies ++= Seq(
      "org.postgresql" % "postgresql" % "42.2.20",
      "io.github.scala-bones" %% "scatonic-ideal" % "0.3.0",
      "co.fs2" %% "fs2-core" % "3.0.0",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, testSchemas % "test->compile")

lazy val dbDoobie = (project in file("db-interpreters/doobie"))
  .settings(
    commonSettings,
    name := "Bones Doobie",
    libraryDependencies ++= Seq(
      "org.tpolecat" %% "doobie-core" % doobieVersion,
      "org.tpolecat" %% "doobie-postgres" % doobieVersion,
//      "org.postgresql" % "postgresql" % "42.2.16",
      "co.fs2" %% "fs2-core" % "3.0.0",
      "io.github.scala-bones" %% "scatonic-ideal" % "0.3.0",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, dbJdbc, testSchemas % "test->compile")

lazy val restHttpCommon = (project in file("rest-interpreters/http-common"))
  .settings(
    commonSettings,
    name := "Bones Http Common",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, swaggerOas3)

lazy val http4sVersion = "1.0.0-M21"
lazy val restHttp4s = (project in file("rest-interpreters/http4s-interpreter"))
  .settings(
    commonSettings,
    name := "Bones Http4s Server",
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-dsl" % http4sVersion,
      "org.http4s" %% "http4s-blaze-server" % http4sVersion,
      "org.http4s" %% "http4s-circe" % http4sVersion,
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, restHttpCommon, swaggerOas3, jsonCirce % "test->compile")

val AkkaVersion = "2.6.14"
val AkkaHttpVersion = "10.2.4"
lazy val restAkkaHttp = (project in file("rest-interpreters/akka-http-interpreter"))
  .settings(
    commonSettings,
    name := "Bones Akka Http Server",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-stream" % AkkaVersion,
      "com.typesafe.akka" %% "akka-http" % AkkaHttpVersion,
      "com.typesafe.akka" %% "akka-http-spray-json" % AkkaHttpVersion,
      "com.typesafe.akka" %% "akka-actor-typed" % AkkaVersion,
      "org.scalacheck" %% "scalacheck" % "1.15.2" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, jsonSpray, restHttpCommon, swaggerOas3)
lazy val dbSlick = (project in file("db-interpreters/slick"))
  .settings(
    commonSettings,
    name := "Bones Slick",
    libraryDependencies ++= Seq(
      "com.typesafe.slick" %% "slick" % "3.3.3",
      "io.underscore" %% "slickless" % "0.3.6",
      "org.slf4j" % "slf4j-nop" % "1.7.30",
      "org.scala-lang" % "scala-reflect" % "2.13.5",
      "org.scalacheck" %% "scalacheck" % "1.15.2" % Test,
      "org.scalatest" %% "scalatest" % "3.2.8" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core)
lazy val awsLambda = (project in file("rest-interpreters/aws-lambda"))
  .settings(
    commonSettings,
    name := "Bones AWS Lambda Server",
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
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
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
      "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
    )
  )
  .dependsOn(core, jsonCirce, testSchemas % "test")
lazy val examples = (project in file("examples/http4s-examples"))
  .settings(
    commonSettings,
    name := "Bones Examples",
    libraryDependencies ++= Seq(
      "io.swagger.core.v3" % "swagger-jaxrs2" % "2.1.9",
      "io.swagger" % "swagger-parser" % "1.0.54",
      "org.slf4j" % "slf4j-simple" % "1.7.30",
      "com.zaxxer" % "HikariCP" % "4.0.3",
      "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
      "org.scalatest" %% "scalatest" % "3.2.3" % Test,
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
    jsonCirce,
    bson,
    testSchemas % "test",
    scalacheck % "test"
  )
lazy val protobufIntegrationTest =
  (project in file("interchange-format-interpreters/javapb-integration-test"))
    .settings(
      commonSettings,
      name := "Bones JavaPB Integration Test",
      libraryDependencies ++= Seq(
        "com.google.protobuf" % "protobuf-java" % "3.24.4",
        "org.scalacheck" %% "scalacheck" % "1.15.3" % Test,
        "org.scalatest" %% "scalatest" % "3.2.3" % Test,
        "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test
      )
    )
    .dependsOn(protobuf % "test", testSchemas % "test")

//enablePlugins(TutPlugin)

testOptions in Test += Tests.Argument("-oF")

resolvers += "tPoleCat" at "https://dl.bintray.com/tpolecat/maven/"
resolvers += "wolfendale" at "https://dl.bintray.com/wolfendale/maven/"
