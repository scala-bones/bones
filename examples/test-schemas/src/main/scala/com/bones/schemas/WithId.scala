package com.bones.schemas

import com.bones.data.values.{DefaultValues, ScalaCoreInjectedSugar}
import com.bones.data.{
  ConcreteValue,
  SwitchEncoding,
  KeyValueDefinition,
  CoproductSwitch,
  KvpSingleValueHead,
  Sugar
}
import com.bones.schemas.Schemas.AllSupported

object WithLongId extends WithId[DefaultValues] {
  import com.bones.syntax._

  val idDefinition = ("id", long(lv.positive))

  val allSupportedWithId =
    schemaWithId[AllSupported, Long](idDefinition, Schemas.allSupportCaseClass)

}
trait WithId[ALG[_]] extends Sugar[ALG] {

  def schemaWithId[A: Manifest, ID: Manifest](
    idDefinition: (String, ALG[ID]),
    schema: ConcreteValue[ALG, A]) = {
    (idDefinition :: schema :><: kvpNil).tupled[(ID, A)]
  }
}
