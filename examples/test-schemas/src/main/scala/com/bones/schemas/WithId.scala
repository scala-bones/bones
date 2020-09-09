package com.bones.schemas

import com.bones.data.values.DefaultValues
import com.bones.data.{KvpCollectionValue, KvpNil, Sugar}
import com.bones.schemas.Schemas.AllSupported

object WithLongId extends WithId[DefaultValues] {
  import com.bones.syntax._

  val idDefinition = ("id", long(lv.positive))

  val allSupportedWithId =
    schemaWithId[AllSupported, Long](idDefinition, Schemas.allSupportCaseClass.asValue)

}
trait WithId[ALG[_]] extends Sugar[ALG] {

  def schemaWithId[A: Manifest, ID: Manifest](
    idDefinition: (String, ALG[ID]),
    schema: KvpCollectionValue[ALG, A]) = {
    (idDefinition :: schema.kvpCollection :: KvpNil[ALG]).tupled[(ID, A)]
  }
}
