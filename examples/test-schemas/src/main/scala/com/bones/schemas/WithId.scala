package com.bones.schemas

import com.bones.data.custom.{AllCustomAlgebras, ScalaCoreInjectedSugar}
import com.bones.data.{BonesSchema, HListConvert, KeyValueDefinition, KvpCoproductConvert, KvpSingleValueHead, Sugar}
import com.bones.schemas.Schemas.AllSupported

object WithLongId extends WithId[AllCustomAlgebras] {
  import com.bones.syntax._

  val idDefinition = ("id", long(lv.positive))

  val allSupportedWithId = schemaWithId[AllSupported, Long](idDefinition, Schemas.allSupportCaseClass)



}
trait WithId[ALG[_]] extends Sugar[ALG] {

  def schemaWithId[A:Manifest, ID:Manifest](idDefinition: (String, ALG[ID]), schema: BonesSchema[ALG, A]) = {
    (idDefinition :: schema :><: kvpNil).tupled[(ID, A)]
  }
}
