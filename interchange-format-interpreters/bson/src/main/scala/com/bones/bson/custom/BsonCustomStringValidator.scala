package com.bones.bson.custom

import cats.data.NonEmptyList
import com.bones.bson.BsonValidatorInterpreter
import com.bones.data.Error
import com.bones.data.Error.RequiredValue
import com.bones.data.custom.CustomStringValue
import com.bones.interpreter.KvpInterchangeFormatValidatorInterpreter.InterchangeFormatValidator
import reactivemongo.bson.BSONValue

trait BsonCustomStringValidator extends InterchangeFormatValidator[CustomStringValue, BSONValue]{
  override def validate[A](alg: CustomStringValue[A]): (Option[BSONValue], List[String]) => Either[NonEmptyList[Error.ExtractionError], A] =
    (bson, path) =>
      bson match {
        case Some(bsonVal) =>
          BsonValidatorInterpreter.extractString(Right(alg), classOf[String])(bsonVal,path)
            .asInstanceOf[Either[NonEmptyList[Error.ExtractionError], A]]
        case None => Left(NonEmptyList.one(RequiredValue(path, Right(alg))))

      }
}
