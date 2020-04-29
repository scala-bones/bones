package com.bones.schemas

import com.bones.data.{BonesSchema, HListConvert, KeyValueDefinition, KvpCoproductConvert}
import com.bones.schemas.Schemas.AllSupported
import com.bones.syntax.{NoAlgebra, kvp, long, lv}

object WithId {

  def schemaWithId[ALG[_], A, ID:Manifest](
                                            idDefinition: KeyValueDefinition[ALG, ID],
                                            schema: BonesSchema[ALG, A]) = schema match {
    case h: HListConvert[ALG, _, _, A] @unchecked =>
      implicit val manifest: Manifest[A] = h.manifestOfA
      (idDefinition >>: h :><: com.bones.syntax.kvpNilCov[ALG]).tupled[(ID, A)]
    case co: KvpCoproductConvert[ALG, _, A] @unchecked =>
      implicit val manifest: Manifest[A] = co.manifestOfA
      (idDefinition >>: co :><: com.bones.syntax.kvpNilCov[ALG]).tupled[(ID, A)]

  }

  val idDefinition = kvp[NoAlgebra, Long]("id", long(lv.positive))

  val allSupportedWithId = schemaWithId[NoAlgebra, AllSupported, Long](idDefinition, Schemas.allSupportCaseClass)


}
