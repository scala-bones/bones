package com.bones.data.custom

import java.time._

import com.bones.data.OptionalKvpValueDefinition
import com.bones.validation.ValidationDefinition.ValidationOp

sealed abstract class JavaTimeValue[A: Manifest] {
  val manifestOfA: Manifest[A] = manifest[A]
}

final case class DurationData(validationOp: ValidationOp[Duration])
  extends JavaTimeValue[Duration]
    with ToOptionalData[Duration]

final case class InstantData(validationOp: ValidationOp[Instant])
  extends JavaTimeValue[Instant]
    with ToOptionalData[Instant]

final case class MonthData(validationOp: ValidationOp[Month])
  extends JavaTimeValue[Month]
    with ToOptionalData[Month]

final case class PeriodData(validationOp: ValidationOp[Period])
  extends JavaTimeValue[Period]
    with ToOptionalData[Period]

final case class ZoneIdData(validationOp: ValidationOp[ZoneId])
  extends JavaTimeValue[ZoneId]
    with ToOptionalData[ZoneId]

final case class ZoneOffsetData(validationOp: ValidationOp[ZoneOffset])
  extends JavaTimeValue[ZoneOffset]
    with ToOptionalData[ZoneOffset]

/** Syntactic sugar to wrap the data definition to allow 'optional' syntax on a KvpValue. */
trait ToOptionalData[B] { self: JavaTimeValue[B] =>
  private implicit val manifestOfB: Manifest[B] = self.manifestOfA
  val optional: OptionalKvpValueDefinition[JavaTimeValue, B] = OptionalKvpValueDefinition[JavaTimeValue, B](Right(self))
}



