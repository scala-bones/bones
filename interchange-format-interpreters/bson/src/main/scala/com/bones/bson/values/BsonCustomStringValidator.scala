package com.bones.bson.values

import com.bones.bson.BsonPrimitiveValidator
import com.bones.data.Error.{ExtractionErrors, RequiredValue}
import com.bones.data.values.CustomStringValue
import com.bones.interpreter.validator.{InterchangeFormatValidatorValue, OptionalInputValidator}
import reactivemongo.bson.BSONValue

trait BsonCustomStringValidator
    extends InterchangeFormatValidatorValue[CustomStringValue, BSONValue] {

  override def createValidator[A](
    alg: CustomStringValue[A]): OptionalInputValidator[String, CustomStringValue, A, BSONValue] =
    (bson, path) =>
      bson match {
        case Some(bsonVal) =>
          BsonPrimitiveValidator
            .extractString("String")
            .validateWithPath(bsonVal, path)
            .asInstanceOf[Either[ExtractionErrors[String], A]]
        case None => Left(List(RequiredValue(path, alg.typeName)))

    }
}
