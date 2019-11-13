package com.bones.perf

import java.nio.charset.Charset

import cats.implicits._
import com.bones.argonaut.{ArgonautEncoderInterpreter, ArgonautValidatorInterpreter}
import com.bones.bson.{BsonEncoderInterpreter, BsonValidatorInterpreter}
import com.bones.circe.{CirceEncoderInterpreter, CirceValidatorInterpreter}
import com.bones.interpreter.{EncodeToJValueInterpreter, ValidatedFromJObjectInterpreter}
import com.bones.protobuf.{ProtobufSequentialInputInterpreter, ProtobufSequentialOutputInterpreter}
import com.bones.scalacheck.Scalacheck
import com.bones.schemas.Schemas.allSupportCaseClass
import com.bones.sjson.JsonStringEncoderInterpreter
import net.liftweb.json.compactRender

object InterchangePerformanceTests extends App {

  val schema = allSupportCaseClass

  val scalaCheckInterpreter = Scalacheck.valueDefinition(schema)

  val directInterpreter = JsonStringEncoderInterpreter.isoEncoder.fAtoString(schema)

  val circeValidator = CirceValidatorInterpreter.isoInterpreter.byteArrayFuncFromSchema(schema, Charset.defaultCharset())
  val circeEncoder = CirceEncoderInterpreter.isoInterpreter.fromSchema(schema)

  val argValidator = ArgonautValidatorInterpreter.isoInterpreter.byteArrayFuncFromSchema(schema, Charset.defaultCharset())
  val argEncoder = ArgonautEncoderInterpreter.isoInterpreter.fromSchema(schema)

  val bsonValidator = BsonValidatorInterpreter.fromSchema(schema)
  val bsonEncoder = BsonEncoderInterpreter.fromSchema(schema)

  val liftJsonValidator = ValidatedFromJObjectInterpreter.isoInterpreter.fromSchema(schema)
  val liftJsonEncoder = EncodeToJValueInterpreter.isoInterpreter.fromSchema(schema)

  val protoValidator = ProtobufSequentialInputInterpreter.fromBytes(schema)
  val protoEncoder = ProtobufSequentialOutputInterpreter.encodeToBytes(schema)



  val objects = Range(0,5000).toList
    .flatMap(_ => scalaCheckInterpreter.sample)

  System.gc()
  val directEncoderStart = System.currentTimeMillis()
  val jsonObjects = objects.map(i => directInterpreter(i))
  val directEncoderEnd = System.currentTimeMillis()
  val success = jsonObjects.traverse(io.circe.parser.parse).isRight
  val jsonSize = jsonObjects.map(_.getBytes.length).sum
  println(s"direct encoder size: ${jsonSize} success: ${success} time: ${directEncoderEnd - directEncoderStart}")
  System.gc()

  //bson
  val bsonEncoderStart = System.currentTimeMillis()
  val bsonObjects = objects.map(o => BsonEncoderInterpreter.bsonResultToBytes(bsonEncoder(o)))
  val bsonEncoderEnd = System.currentTimeMillis()
  val bsonSize = bsonObjects.map(_.length).sum
  println(s"bson encoder size: ${bsonSize} time: ${bsonEncoderEnd - bsonEncoderStart}")
  System.gc()

  val bsonStart = System.currentTimeMillis()
  val bsonResult = bsonObjects.map(b => bsonValidator(BsonValidatorInterpreter.fromByteArray(b).toOption.get))
  val bsonEnd = System.currentTimeMillis()
  val badBsonResults = bsonResult.indices.filterNot(i => {
    (bsonResult.get(i).flatMap(_.toOption), objects.get(i)) match {
      case (Some(o1), Some(o2)) => o1.fancyEquals(o2)
      case (None, None) => true
      case _ => false
    }})
  val bsonSuccess = badBsonResults.isEmpty
  println(s"bson validator success: ${bsonSuccess} time: ${bsonEnd - bsonStart}")
  System.gc()


  val circeEncoderStart = System.currentTimeMillis()
  objects.map(o => circeEncoder.apply(o).noSpaces)
  val circeEncoderEnd = System.currentTimeMillis()
  println(s"circe encoder time: ${circeEncoderEnd - circeEncoderStart}")
  System.gc()

  val circeStart = System.currentTimeMillis()
  val result = jsonObjects.map(j => circeValidator.apply(j.getBytes))
  val circeStop = System.currentTimeMillis()
  val badCirceResults = result.indices.filterNot(i => {
    (result.get(i).flatMap(_.toOption), objects.get(i)) match {
      case (Some(o1), Some(o2)) => o1.fancyEquals(o2)
      case (None, None) => true
      case _ => false
    }})
  val circeSuccess = badCirceResults.isEmpty


  println(s"Circe Validator Success: ${circeSuccess} Time: ${circeStop - circeStart}")
  System.gc()

  val argoEncoderStart = System.currentTimeMillis()
  objects.map(o => argEncoder.apply(o).nospaces)
  val argEncoderEnd = System.currentTimeMillis()
  println(s"argo encoder time: ${argEncoderEnd - argoEncoderStart}")
  System.gc()

  val argoStart = System.currentTimeMillis()
  val argResult = jsonObjects.map(j => argValidator.apply(j.getBytes))
  val argoStop = System.currentTimeMillis()
  val badArgoResults = argResult.indices.filterNot(i => {
    (argResult.get(i).flatMap(_.toOption), objects.get(i)) match {
      case (Some(o1), Some(o2)) => o1.fancyEquals(o2)
      case (None, None) => true
      case _ => false
    }})
  val argoSuccess = badCirceResults.isEmpty
  println(s"Argo Validator Success: ${argoSuccess} Time: ${argoStop - argoStart}")
  System.gc()

  val liftJsonEncoderStart = System.currentTimeMillis()
  val liftEncoder = objects.map(o => compactRender(liftJsonEncoder.apply(o)))
  val liftJsonEncoderEnd = System.currentTimeMillis()
  println(s"Lift Json Encoder Time: ${liftJsonEncoderEnd - liftJsonEncoderStart}")
  System.gc()

  val liftJsonValidatorStart = System.currentTimeMillis()
  val liftValidatorResult =
    jsonObjects.map(j => liftJsonValidator.apply(EncodeToJValueInterpreter.stringToJson(j)))
  val liftJsonValidatorEnd = System.currentTimeMillis()
  val badListResults = liftValidatorResult.indices.filterNot(i => {
    (liftValidatorResult.get(i).flatMap(_.toOption), objects.get(i)) match {
      case (Some(o1), Some(o2)) => o1.fancyEquals(o2)
      case (None, None) => true
      case _ => false
    }})
  val liftSuccess = badListResults.isEmpty
  println(s"Lift Json Validator Success: ${liftSuccess} Time: ${liftJsonValidatorEnd - liftJsonValidatorStart}")
  System.gc()

  val protoEncodeStart = System.currentTimeMillis()
  val peResult = objects.map(protoEncoder.apply(_))
  val protoEncodeEnd = System.currentTimeMillis()
  val protoSize = peResult.map(_.length).sum
  println(s"Proto Encode Size: ${protoSize} time: ${protoEncodeEnd - protoEncodeStart}")
  System.gc()

  val protoValidateStart = System.currentTimeMillis()
  val pe = peResult.map(pe => protoValidator.apply(pe))
  val protoValidateEnd = System.currentTimeMillis()
  println(s"Proto Validate time: ${protoValidateEnd - protoValidateStart}")
  System.gc()








}
