package com.bones.schemas

import com.bones.data.Sugar
import com.bones.data.values.{ScalaCoreSugar, ScalaCoreValue}

object ScalaCoreSchema {

  val alg = new ScalaCoreSugar with Sugar[String, ScalaCoreValue]
  import alg._

  object TestEnum extends Enumeration {
    type TestEnum = Value
    val One, Two, Three = Value
  }

  case class AllScalaCore(
    i: Int,
    l: Long,
    s: Short,
    b: Boolean,
    str: String,
    f: Float,
    e: TestEnum.Value,
    bd: BigDecimal,
    ba: Array[Byte])
  val scalaCoreHList = ("int", int()) ::
    ("long", long()) ::
    ("short", short()) ::
    ("boolean", boolean()) ::
    ("string", string()) ::
    ("float", float()) ::
    ("enumeration", enumeration[TestEnum.type, TestEnum.Value](TestEnum)) ::
    ("bigDecimal", bigDecimal()) ::
    ("byteArray", byteArray()) ::
    kvpNil

  val schema = scalaCoreHList.convert[AllScalaCore]

  val idSchema = (("id", int()) :: kvpNil).tupled[Tuple1[Int]]

}
