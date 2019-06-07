package com.bones.sjson

import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.util.UUID

import com.bones.data.KeyValueDefinition
import com.bones.data.Value._
import com.bones.interpreter.KvpOutputInterpreter
import shapeless.{HList, Nat}


object StringOutInterpreter {


  def kvpHList[H <: HList, HL <: Nat](group: KvpHList[H, HL]): Unit = {
    group match {
      case KvpNil                                          => ???
      case op: KvpSingleValueHead[h, t, tl, a]             => ???
      case op: KvpHListHead[a, al, h, hl, t, tl]           => ???
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xl, xll] => ???
    }
  }

  def valueDefinition[A](fgo: KvpValue[A]): Unit =
    fgo match {
      case op: OptionalKvpValueDefinition[a] => ???
      case ob: BooleanData                => ???
      case rs: StringData                 => ???
      case ri: LongData                   => ???
      case uu: UuidData                   => ???
      case dd: DateTimeData               => ???
      case bd: BigDecimalData             => ???
      case ld: ListData[t]                => ???
      case ed: EitherData[a, b]           => ???
      case ba: ByteArrayData              => ???
      case esd: EnumerationStringData[a]  => ???
      case kvp: KvpHListValue[h, hl]      => ???
      case x: HListConvert[a, al, b]      => ???
      case s: SumTypeData[a, b]           => ???
    }




}