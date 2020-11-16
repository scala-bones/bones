package com.bones.bson.values

import com.bones.bson.BsonPrimitiveValidator
import com.bones.data.Error.{ExtractionErrors, RequiredValue}
import com.bones.data.values.CustomStringValue
import com.bones.interpreter.InterchangeFormatValidatorValue
import reactivemongo.bson.BSONValue

trait BsonCustomStringValidator
    extends InterchangeFormatValidatorValue[CustomStringValue, BSONValue] {

  override def validate[A](alg: CustomStringValue[A])
    : (Option[BSONValue], List[String]) => Either[ExtractionErrors[String], A] =
    (bson, path) =>
      bson match {
        case Some(bsonVal) =>
          BsonPrimitiveValidator
            .extractString(Right(alg), "String")(bsonVal, path)
            .asInstanceOf[Either[ExtractionErrors[String], A]]
        case None => Left(List(RequiredValue(path, alg.typeName)))

    }
}
