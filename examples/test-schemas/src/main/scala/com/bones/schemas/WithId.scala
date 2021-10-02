package com.bones.schemas

import com.bones.data.values.DefaultValues
import com.bones.data.{KvpCollectionValue, KvpNil, Sugar}
import com.bones.schemas.Schemas.AllSupported

object WithLongId extends WithId[String, DefaultValues] {
  import com.bones.syntax._

  val idDefinition = ("id", long(lv.positive))

  val allSupportedWithId =
    schemaWithId[AllSupported, Long](idDefinition, Schemas.allSupportCaseClass.asValue)

}
trait WithId[K, ALG[_]] {

  def schemaWithId[A: Manifest, ID: Manifest](
    idDefinition: (String, ALG[ID]),
    schema: KvpCollectionValue[String, ALG, A]
  ) = {
    (idDefinition :: schema.kvpCollection :: KvpNil[String, ALG]()).tupled[(ID, A)]
  }
}
