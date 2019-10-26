package com.bones.scalacheck

import com.bones.data.KeyValueDefinition
import com.bones.data._
import com.bones.validation.ValidationDefinition.StringValidation.{MaxLength, Trimmed}
import com.bones.validation.ValidationDefinition.ValidationOp
import org.scalacheck.Gen
import shapeless.ops.hlist.IsHCons
import shapeless.{::, HList, HNil, Nat, Succ}

object GenGadt {


  def genMaxStringLength: Gen[MaxLength] = for {
    len <- Gen.choose(0,5000)
  } yield MaxLength(len)

  def genTrimmed: Gen[Trimmed.type] = Gen.const(Trimmed)

  def genStringValidation: Gen[ValidationOp[String]] =
    Gen.oneOf(genMaxStringLength, genTrimmed)

  def genStringData: Gen[StringData] = for {
    validation <- genStringValidation
  } yield StringData(List(validation))

  def genDoubleData: Gen[DoubleData] = Gen.const(DoubleData(List.empty))

  def genKeys = Gen.oneOf(Scalacheck.loremIpsumWords)

  val types: Gen[String] = Gen.oneOf("String", "Double", "Object")
  val false90Percent: Gen[Boolean] = Gen.frequency((1,true), (9,false))

  def genHListValue(): Gen[KvpHListValue[_<:HList, _<:Nat]] = for {
    strHead <- genStringData
    t <- types
    hList <- nextGen(false, "first", strHead, KvpNil, t)
  } yield KvpHListValue(hList, List.empty)

  def nextGen[A,H<:HList,N<:Nat](done: Boolean, key: String, newHead: KvpValue[A], tail: KvpHList[H,N], nextType: String): Gen[KvpHList[_<:HList,_<:Nat]] = {

    val isHCons = implicitly[IsHCons.Aux[A :: H, A, H]]
    val thisValue: KvpHList[A::H,Succ[N]] = KvpSingleValueHead(KeyValueDefinition(key, newHead), List.empty, tail, isHCons)
    if (done) {
      Gen.const(thisValue)
    } else {
      nextType match {
        case "String" => genStringData.flatMap(sd => {
          genKvpSingleValueHead(sd, thisValue)
        })
        case "Double" => genDoubleData.flatMap(dd => {
          genKvpSingleValueHead(dd, thisValue)
        })
        case "Object" => {
          genKvpSingleValueHead(StringData(List.empty), KvpNil).flatMap(kvpHList => {
            val next = KvpHListValue(kvpHList, List.empty)
//            val nextCons = implicitly[IsHCons.Aux[String::HNil, String, HNil]]
            genKeys.map(objKey => {
              KvpSingleValueHead(KeyValueDefinition(key, next), List.empty, thisValue, null)
            })
          })
        }
      }
    }
  }

  def genKvpSingleValueHead[A, H<:HList,N<:Nat](newHead: KvpValue[A], tail: KvpHList[H,N]): Gen[KvpHList[_<:HList, _<:Nat]] = {
    for {
      key <- genKeys
      t <- types
      done <- false90Percent
      next <- nextGen(done, key, newHead, tail, t)
    } yield {
      next
    }
  }












}
