package com.bones.jdbc

import com.bones.data.KeyValueDefinition.CoproductDataDefinition
import com.bones.data._
import com.bones.jdbc.DbUtil._
import com.bones.syntax.NoAlgebra
import shapeless.{Coproduct, HList, Inl, Inr, Nat}

/** Responsible for determining the column names to be used in the select statement */
object ColumnNameInterpreter {

  type ColumnName = String
  type Key = String

  trait CustomInterpreter[ALG[_]] {
    def keyToColumnNames[A](alg: ALG[A]): Key => List[ColumnName]
  }

  case object CustomInterpreterNoAlgebra extends CustomInterpreter[NoAlgebra] {
    def keyToColumnNames[A](alg: NoAlgebra[A]): Key => List[ColumnName] =
      sys.error("unreachable code")
  }

  def kvpHList[ALG[_], H <: HList, HL <: Nat](
    group: KvpHList[ALG, H, HL],
    customInterpreter: CustomInterpreter[ALG]): List[ColumnName] = {
    group match {
      case nil: KvpNil[_] => List.empty
      case op: KvpSingleValueHead[ALG, h, t, tl, a] @unchecked =>
        val headList =
          determineValueDefinition(
            op.fieldDefinition.dataDefinition,
            customInterpreter: CustomInterpreter[ALG])(op.fieldDefinition.key)
        val tailList = kvpHList(op.tail, customInterpreter: CustomInterpreter[ALG])
        headList ::: tailList
      case op: KvpHListHead[ALG, a, al, h, hl, t, tl] @unchecked =>
        kvpHList(op.head, customInterpreter) ::: kvpHList(op.tail, customInterpreter)
      case op: KvpConcreteTypeHead[ALG, a, ht, nt] =>
        fromBonesSchema(op.bonesSchema, customInterpreter)
    }
  }

  def fromBonesSchema[ALG[_], A](
    bonesSchema: BonesSchema[ALG, A],
    customInterpreter: CustomInterpreter[ALG]): List[ColumnName] =
    bonesSchema match {
      case hList: HListConvert[ALG, h, n, a] @unchecked => kvpHList(hList.from, customInterpreter)
      case co: KvpCoproductConvert[ALG, c, a] @unchecked =>
        valueDefinition(co, customInterpreter)("")
    }

  type CoproductName = String

  protected def kvpCoproduct[ALG[_], C <: Coproduct](
    kvpCo: KvpCoproduct[ALG, C],
    customInterpreter: CustomInterpreter[ALG]): List[ColumnName] =
    kvpCo match {
      case co: KvpCoNil[_] => List.empty
      case co: KvpSingleValueLeft[ALG, l, r] =>
        val head = determineValueDefinition(co.kvpValue, customInterpreter)("")
        val tail = kvpCoproduct(co.kvpTail, customInterpreter)
        head ::: tail
    }

  private val keyToColumNames: Key => List[ColumnName] = key => List(camelToSnake(key))

  def determineValueDefinition[ALG[_], A](
    kvp: CoproductDataDefinition[ALG, A],
    customInterpreter: CustomInterpreter[ALG]): Key => List[ColumnName] =
    kvp match {
      case Left(kvp)  => valueDefinition(kvp, customInterpreter)
      case Right(alg) => customInterpreter.keyToColumnNames(alg)
    }

  def valueDefinition[ALG[_], A](
    fgo: KvpValue[A],
    customInterpreter: CustomInterpreter[ALG]): Key => List[ColumnName] =
    fgo match {
      case op: OptionalKvpValueDefinition[ALG, a] @unchecked =>
        determineValueDefinition(op.valueDefinitionOp, customInterpreter)
      case ob: BooleanData                 => keyToColumNames
      case rs: StringData                  => keyToColumNames
      case sd: ShortData                   => keyToColumNames
      case id: IntData                     => keyToColumNames
      case ri: LongData                    => keyToColumNames
      case uu: UuidData                    => keyToColumNames
      case ld: LocalDateData               => keyToColumNames
      case dd: LocalDateTimeData           => keyToColumNames
      case fd: FloatData                   => keyToColumNames
      case dd: DoubleData                  => keyToColumNames
      case bd: BigDecimalData              => keyToColumNames
      case ld: ListData[ALG, t] @unchecked => keyToColumNames
      case ba: ByteArrayData               => keyToColumNames
      case ed: EitherData[ALG, a, b] @unchecked =>
        key =>
          {
            val baseName = camelToSnake(key)
            List("left_" + baseName, "right_" + baseName)
          }
      case esd: EnumerationData[e, a] => keyToColumNames

      case kvp: KvpHListValue[ALG, h, hl] @unchecked =>
        _ =>
          kvpHList(kvp.kvpHList, customInterpreter)
      case x: HListConvert[ALG, a, al, b] @unchecked =>
        _ =>
          kvpHList(x.from, customInterpreter)
      case x: KvpCoproductConvert[ALG, c, a] @unchecked =>
        _ =>
          {
            val columnNames = kvpCoproduct(x.from, customInterpreter)
            "dtype" :: columnNames
          }
    }

}
