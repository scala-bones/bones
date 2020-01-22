package com.bones.data.custom

import java.time._

import com.bones.data.{AlgToCollectionData, HasManifest, ListData}
import com.bones.validation.ValidationDefinition.ValidationOp

sealed abstract class JavaTimeValue[A: Manifest] extends HasManifest[A] {
  val manifestOfA: Manifest[A] = manifest[A]
}

final case class DurationData(validationOp: ValidationOp[Duration])
  extends JavaTimeValue[Duration]
    with AlgToCollectionData[JavaTimeValue, Duration, DurationData]

final case class InstantData(validationOp: ValidationOp[Instant])
  extends JavaTimeValue[Instant]
    with AlgToCollectionData[JavaTimeValue, Instant, InstantData]

final case class MonthData(validationOp: ValidationOp[Month])
  extends JavaTimeValue[Month]
    with AlgToCollectionData[JavaTimeValue, Month, MonthData]

final case class MonthDay(validationOp: ValidationOp[MonthDay])
  extends JavaTimeValue[MonthDay]
    with AlgToCollectionData[JavaTimeValue, MonthDay, MonthDay]

final case class PeriodData(validationOp: ValidationOp[Period])
  extends JavaTimeValue[Period]
    with AlgToCollectionData[JavaTimeValue, Period, PeriodData]

final case class ZoneIdData(validationOp: ValidationOp[ZoneId])
  extends JavaTimeValue[ZoneId]
    with AlgToCollectionData[JavaTimeValue, ZoneId, ZoneIdData]

final case class ZoneOffsetData(validationOp: ValidationOp[ZoneOffset])
  extends JavaTimeValue[ZoneOffset]
    with AlgToCollectionData[JavaTimeValue, ZoneOffset, ZoneOffsetData]


