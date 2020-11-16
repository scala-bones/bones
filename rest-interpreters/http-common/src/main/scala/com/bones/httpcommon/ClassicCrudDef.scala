package com.bones.httpcommon

import com.bones.data.values.ScalaCoreValue
import com.bones.data.{KvpCollection, KvpNil}
import shapeless.HNil

case class ClassicCrudDef[ALG[_], A: Manifest, ID: Manifest, CT, E, PE, K](
  interpreterConfig: InterpreterConfig[ALG, ID, CT],
  path: String,
  schema: KvpCollection[K, ALG, A],
  pathStringToId: String => Either[PE, ID],
  pathErrorSchema: KvpCollection[K, ALG, PE],
  errorSchema: KvpCollection[K, ALG, E],
  idKey: K,
  scvToAlg: ScalaCoreValue[_] => ALG[_]
) {

  val idSchema: KvpCollection[K, ALG, ID] =
    ((idKey, interpreterConfig.idDefinition) :: new KvpNil[K, ALG]).encodedHead()

  val schemaWithId: KvpCollection[K, ALG, (ID, A)] =
    ((idKey, interpreterConfig.idDefinition) :: schema :: KvpNil[K, ALG]()).tupled[(ID, A)]

  val httpData: HttpData[ALG, A, A, E, ID, CT, K] =
    HttpData(interpreterConfig, schema, schema, errorSchema, scvToAlg)

  val httpDataWithId: HttpData[ALG, (ID, A), (ID, A), HNil, ID, CT, K] =
    HttpData(interpreterConfig, schemaWithId, schemaWithId, KvpCollection.empty, scvToAlg)

  val httpId: HttpData[ALG, ID, ID, HNil, ID, CT, K] =
    HttpData(interpreterConfig, idSchema, idSchema, KvpCollection.empty, scvToAlg)

  val httpPathError: HttpData[ALG, HNil, PE, HNil, ID, CT, K] =
    HttpData(interpreterConfig, KvpCollection.empty, pathErrorSchema, KvpCollection.empty, scvToAlg)

}
