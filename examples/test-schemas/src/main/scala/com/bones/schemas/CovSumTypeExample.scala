package com.bones.schemas

import com.bones.syntax._

object CovSumTypeExample {

  object Quality extends Enumeration {
    type Quality = Value
    val Mint, Excellent, Good, Fair, Poor = Value
    val bonesSchema = enumeration[Quality.type, Quality.Value](Quality)
  }

  object MusicMedium {
    val baseFields =
      ("name", string(sv.words)) ::
        kvpNil

    // Note the order needs to be the order in which they are defined below
    val bonesSchema =
      (Album.bonesSchema :<+: CompactDisc.bonesSchema :<+: Digital.bonesSchema :<+: kvpCoNil)
        .convert[MusicMedium]
//    type MusicMedium = Digital :+: CompactDisc :+: Album :+: CNil
  }
  sealed trait MusicMedium {
    val name: String
  }

  object Album {
    private val fields =
      MusicMedium.baseFields :::
        ("albumQuality", Quality.bonesSchema) ::
        ("coverQuality", Quality.bonesSchema) ::
        kvpNil

    val bonesSchema = fields.convert[Album]
  }
  case class Album(name: String, albumQuality: Quality.Quality, coverQuality: Quality.Quality)
      extends MusicMedium

  object CompactDisc {
    private val fields =
      MusicMedium.baseFields :::
        ("cdQuality", Quality.bonesSchema) ::
        ("caseQuality", Quality.bonesSchema) ::
        kvpNil

    val bonesSchema = fields.convert[CompactDisc]
  }

  case class CompactDisc(name: String, cdQuality: Quality.Quality, caseQuality: Quality.Quality)
      extends MusicMedium

  object Digital {
    private val fields =
      MusicMedium.baseFields :::
        ("format", string()) ::
        kvpNil
    val bonesSchema = fields.convert[Digital]
  }
  case class Digital(name: String, format: String) extends MusicMedium

  object Item {
    val fields =
      ("artist", string(sv.words)) ::
//      kvp("medium", MusicMedium.bonesSchema) ::
        kvpNil
//    val bonesSchema = fields.convert[Item]
  }
  case class Item(artist: String, medium: MusicMedium)

}
