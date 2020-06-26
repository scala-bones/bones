package com.bones.jdbc

import java.util.{Calendar, TimeZone}

import com.bones.data._
import com.bones.data.custom.AnyAlg
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{HList, Nat}

object FindInterpreter {

  val utcCalendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
  type FieldName = String
  type Path = List[String]

}

object TableName {

  def getTableName[ALG[_], B](dc: BonesSchema[ALG, B]): String = dc match {
    case t: HListConvert[_, a, al, b] @unchecked =>
      camelToSnake(t.manifestOfA.runtimeClass.getSimpleName)
  }
}

object FieldNames {

  trait CustomFieldNamesInterpreter[ALG[_]] {
    def fieldNames[A](alg: ALG[A]): List[String]
  }

  def fromCustomSchema[ALG[_], A](
    dc: BonesSchema[ALG, A],
    customFieldNamesInterpreter: CustomFieldNamesInterpreter[ALG]): List[String] =
    dc match {
      case t: HListConvert[ALG, a, al, b] @unchecked =>
        kvpHList(t.from, customFieldNamesInterpreter)
    }

  def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHList[ALG, H, HL],
    customFieldNamesInterpreter: CustomFieldNamesInterpreter[ALG]): List[String] =
    group match {
      case nil: KvpNil[_] => List.empty
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        List(camelToSnake(op.fieldDefinition.key)) ::: kvpHList(
          op.tail,
          customFieldNamesInterpreter)
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] @unchecked => {
        val headList = op.bonesSchema match {
          case hList: HListConvert[ALG, h, n, a] =>
            kvpHList(hList.from, customFieldNamesInterpreter)
          case co: KvpCoproductConvert[ALG, c, a] => ???
        }
        headList ::: kvpHList(op.tail, customFieldNamesInterpreter)
      }
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        kvpHList(op.head, customFieldNamesInterpreter) ::: kvpHList(
          op.tail,
          customFieldNamesInterpreter)
    }

  def determineValueDefinition[ALG[_], A](
    valueDefinitionOp: Either[KvpCollection[ALG,A], AnyAlg[A]],
    customFieldNamesInterpreter: CustomFieldNamesInterpreter[ALG]): List[String] =
    valueDefinitionOp match {
      case Left(kvp)  => valueDefinition(kvp, customFieldNamesInterpreter)
      case Right(_) => List.empty
    }

  def valueDefinition[ALG[_], A](
    fgo: KvpCollection[ALG,A],
    customFieldNamesInterpreter: CustomFieldNamesInterpreter[ALG]): List[String] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, a] @unchecked =>
        determineValueDefinition(op.valueDefinitionOp, customFieldNamesInterpreter)
      case ld: ListData[ALG, t] @unchecked      => List.empty
      case ed: EitherData[ALG, a, b] @unchecked => List.empty
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        kvpHList(kvp.kvpHList, customFieldNamesInterpreter)
      case x: HListConvert[ALG, _, _, _] @unchecked => kvpHList(x.from, customFieldNamesInterpreter)
    }

}
