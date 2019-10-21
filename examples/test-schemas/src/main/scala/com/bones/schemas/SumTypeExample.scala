package com.bones.schemas

import com.bones.schemas.SumTypeExample.Quality.Quality
import com.bones.syntax._
import shapeless.{:+:, CNil}

object SumTypeExample {

  object Quality extends Enumeration {
    type Quality = Value
    val Mint, Excellent, Good, Fair, Poor = Value
    val bonesSchema = enumeration[Quality.type, Quality.Value](Quality)
  }

  object MusicMedium {
    val baseFields =
      kvp("name", string(sv.words)) ::
      kvpNil
    val bonesSchema = Digital.bonesSchema :+: CompactDisc.bonesSchema :+: Album.bonesSchema :+: kvpCoNil
    type MusicMedium = Digital :+: CompactDisc :+: Album :+: CNil
  }
  sealed trait MusicMedium {
    val name: String
  }

  object Album {
    private val fields =
      MusicMedium.baseFields :::
      kvp("albumQuality", Quality.bonesSchema) ::
      kvp("coverQuality", Quality.bonesSchema) ::
      kvpNil

    val bonesSchema = fields.convert[Album]
  }
  case class Album(name: String, albumQuality: Quality, coverQuality: Quality) extends MusicMedium

  object CompactDisc {
    private val fields =
      MusicMedium.baseFields :::
      kvp("cdQuality", Quality.bonesSchema) ::
      kvp("caseQuality", Quality.bonesSchema) ::
      kvpNil

    val bonesSchema = fields.convert[CompactDisc]
  }

  case class CompactDisc(name: String, cdQuality: Quality, caseQuality: Quality) extends MusicMedium

  object Digital {
    private val fields =
      MusicMedium.baseFields :::
      kvp("format", string()) ::
      kvpNil
    val bonesSchema = fields.convert[Digital]
  }
  case class Digital(name: String, format: String) extends MusicMedium



  object Item {
    val fields =
      kvp("artist", string(sv.words)) ::
      kvpCoproduct("medium", MusicMedium.bonesSchema) ::
      kvpNil
  }
  case class Item(artist: String, medium: MusicMedium.MusicMedium)


}
