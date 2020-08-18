package com.bones.jdbc

import java.util.{Calendar, TimeZone}

import com.bones.data._
import com.bones.data.values.AnyAlg
import com.bones.jdbc.DbUtil.camelToSnake
import shapeless.{HList, Nat}

object FindInterpreter {

  val utcCalendar = Calendar.getInstance(TimeZone.getTimeZone("UTC"))
  type FieldName = String
  type Path = List[String]

}

object TableName {

  def getTableName[ALG[_], B](dc: PrimitiveWrapperValue[ALG, B]): String = dc match {
    case t: SwitchEncoding[_, a, al, b] @unchecked =>
      camelToSnake(t.manifestOfA.runtimeClass.getSimpleName)
    case _ => ??? // TODO
  }
}

object FieldNames {

  trait CustomFieldNamesInterpreter[ALG[_]] {
    def fieldNames[A](alg: ALG[A]): List[String]
  }

  def fromCustomSchema[ALG[_], A](
    dc: PrimitiveWrapperValue[ALG, A],
    customFieldNamesInterpreter: CustomFieldNamesInterpreter[ALG]): List[String] =
    dc match {
      case t: SwitchEncoding[ALG, a, al, b] @unchecked =>
        kvpHList(t.from, customFieldNamesInterpreter)
      case _ => ??? // TODO
    }

  def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHListCollection[ALG, H, HL],
    customFieldNamesInterpreter: CustomFieldNamesInterpreter[ALG]): List[String] =
    group match {
      case nil: KvpNil[_] => List.empty
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        List(camelToSnake(op.fieldDefinition.key)) ::: kvpHList(
          op.tail,
          customFieldNamesInterpreter)
      case op: KvpConcreteValueHead[ALG, a, ht, nt] @unchecked => {
        val headList = op.collection match {
          case hList: SwitchEncoding[ALG, h, n, a] =>
            kvpHList(hList.from, customFieldNamesInterpreter)
          case co: CoproductSwitch[ALG, c, a] => ???
          case _                              => ??? // TODO
        }
        headList ::: kvpHList(op.wrappedEncoding, customFieldNamesInterpreter)
      }
      case op: KvpHListCollectionHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        kvpHList(op.head, customFieldNamesInterpreter) ::: kvpHList(
          op.tail,
          customFieldNamesInterpreter)
    }

  def determineValueDefinition[ALG[_], A](
    valueDefinitionOp: Either[PrimitiveWrapperValue[ALG, A], AnyAlg[A]],
    customFieldNamesInterpreter: CustomFieldNamesInterpreter[ALG]): List[String] =
    valueDefinitionOp match {
      case Left(kvp) => valueDefinition(kvp, customFieldNamesInterpreter)
      case Right(_)  => List.empty
    }

  def valueDefinition[ALG[_], A](
    fgo: PrimitiveWrapperValue[ALG, A],
    customFieldNamesInterpreter: CustomFieldNamesInterpreter[ALG]): List[String] =
    fgo match {
      case op: OptionalValue[ALG, a] @unchecked =>
        determineValueDefinition(op.valueDefinitionOp, customFieldNamesInterpreter)
      case ld: ListData[ALG, t] @unchecked      => List.empty
      case ed: EitherData[ALG, a, b] @unchecked => List.empty
      case kvp: KvpCollectionValue[ALG, h, hl] @unchecked =>
        kvpHList(kvp.kvpCollection, customFieldNamesInterpreter)
      case x: SwitchEncoding[ALG, _, _, _] @unchecked =>
        kvpHList(x.from, customFieldNamesInterpreter)
      case co: CoproductSwitch[ALG, c, a] @unchecked  => ??? // TODO
      case co: CoproductCollection[ALG, a] @unchecked => ??? // TODO
    }

}
