package com.bones.jdbc

import java.util.{Calendar, TimeZone}

import com.bones.data._
import com.bones.jdbc.DbUtil.camelToSnake
import com.bones.syntax.NoAlgebra
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

  object NoAlgebraInterpreter extends CustomFieldNamesInterpreter[NoAlgebra] {
    override def fieldNames[A](alg: NoAlgebra[A]): List[String] = sys.error("unreachable code")
  }

  def fromSchema[A](dc: BonesSchema[NoAlgebra, A]): List[String] =
    fromCustomSchema(dc, NoAlgebraInterpreter)

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
    valueDefinitionOp: Either[KvpValue[A], ALG[A]],
    customFieldNamesInterpreter: CustomFieldNamesInterpreter[ALG]): List[String] =
    valueDefinitionOp match {
      case Left(kvp)  => valueDefinition(kvp, customFieldNamesInterpreter)
      case Right(alg) => customFieldNamesInterpreter.fieldNames(alg)
    }

  def valueDefinition[ALG[_], A](
    fgo: KvpValue[A],
    customFieldNamesInterpreter: CustomFieldNamesInterpreter[ALG]): List[String] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, a] @unchecked =>
        determineValueDefinition(op.valueDefinitionOp, customFieldNamesInterpreter)
      case ob: BooleanData                      => List.empty
      case rs: StringData                       => List.empty
      case id: ShortData                        => List.empty
      case id: IntData                          => List.empty
      case ri: LongData                         => List.empty
      case uu: UuidData                         => List.empty
      case dd: LocalDateData                    => List.empty
      case dd: LocalDateTimeData                => List.empty
      case bd: BigDecimalData                   => List.empty
      case fd: FloatData                        => List.empty
      case dd: DoubleData                       => List.empty
      case ba: ByteArrayData                    => List.empty
      case ld: ListData[ALG, t] @unchecked      => List.empty
      case ed: EitherData[ALG, a, b] @unchecked => List.empty
      case esd: EnumerationData[e, a]           => List.empty
      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        kvpHList(kvp.kvpHList, customFieldNamesInterpreter)
      case x: HListConvert[ALG, _, _, _] @unchecked => kvpHList(x.from, customFieldNamesInterpreter)
    }

}
