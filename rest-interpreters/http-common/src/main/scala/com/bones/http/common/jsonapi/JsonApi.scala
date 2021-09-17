package com.bones.http.common.jsonapi

import com.bones.data.values.DefaultValues
import com.bones.data.{KvpCollection, KvpNil}
import com.bones.syntax._

object JsonApi {

  val typeDescription =
    "The type member is used to describe resource objects that share common attributes and relationships."
  val idDescription = "The unique identifier for this resource object"

  def jsonApiResourceObject[A: Manifest, ID: Manifest](
    kvp: KvpCollection[String, DefaultValues, A],
    idType: DefaultValues[ID]
  ) = {

    val data =
      ("type", string()) ::
        ("id", idType) ::
        ("attributes", kvp.asValue) :<:
        kvpNil

    data

  }

}
