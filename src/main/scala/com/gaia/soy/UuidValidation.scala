package com.gaia.soy

import java.util.UUID

import cats.{Applicative, Functor, Id, Monad, Traverse}
import cats.data.NonEmptyList
import cats.implicits._

import scala.util.Try

object UuidValidation {

  object IsUuid extends ExtractionOp

  case class UuidConversion[F[_]:Applicative:Traverse](key: Key, stringExtraction: Extraction[F,String]) extends Conversion[F,String,UUID] {
    override type I = String

    private def convert(iToO: String, key: => Key): Either[ExtractionErrors, F[UUID]] = try {
      Right(Applicative[F].pure(UUID.fromString(iToO)))
    } catch {
      case _: IllegalArgumentException => Left(NonEmptyList.one(ValidationError(key, IsUuid, Some(iToO))))
    }


    override def extract(stringProducer: RawValueProducer): Either[ExtractionErrors, UUID] =
      stringExtraction.extract(stringProducer).flatMap(fs => {
        Applicative[Either].traverse(fs)(str => convert(str,key))
      })
  }

//  case class OptionalUuidExtracion(key: Key) extends Extraction[Option,UUID] {
//
//  }


//  case class UuidConversion extends ConversionOp[String,UUID] {
//    override def convert(iToO: String): Either[ConversionError[String], UUID] = str =>
//      Try { UUID.fromString(str) }.toEither.left.map(ConversionError())
//
//  }



}
