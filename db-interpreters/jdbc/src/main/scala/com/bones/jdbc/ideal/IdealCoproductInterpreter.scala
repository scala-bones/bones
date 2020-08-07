package com.bones.jdbc.ideal

import com.bones.data.{ConcreteValue, KvpCoNil, KvpCoproductTemplate, KvpSingleValueLeft}
import com.bones.jdbc.DbUtil
import shapeless.Coproduct

trait IdealCoproductInterpreter[ALG[_]]
    extends KvpCoproductTemplate[
      ALG,
      (TableCollection, ColumnName, Option[Description]) => TableCollection] {

  val algInterpreter: IdealValue[ALG]
  def fromCollection[A: Manifest](kvpCollection: ConcreteValue[ALG, A])
    : (TableCollection, ColumnName, Option[Description]) => TableCollection

  override def kvpCoNil(kvpCoproduct: KvpCoNil[ALG])
    : (TableCollection, ColumnName, Option[Description]) => TableCollection =
    (tc, _, _) => tc

  override def kvpSingleValueLeft[A, R <: Coproduct](
    kvpSingleValueLeft: KvpSingleValueLeft[ALG, A, R])
    : (TableCollection, ColumnName, Option[Description]) => TableCollection = {
    val leftF = kvpSingleValueLeft.kvpValue match {
      case Left(kvpCollection: ConcreteValue[ALG, A]) =>
        implicit val manifestOfA = kvpCollection.manifestOfA
        fromCollection[A](kvpCollection)
      case Right(value) =>
        (tableCollection: TableCollection, name: ColumnName, description: Option[Description]) =>
          algInterpreter.columns(value)(
            tableCollection,
            s"${name}_${DbUtil.camelToSnake(kvpSingleValueLeft.manifestL.getClass.getSimpleName)}",
            description)
    }

    (tableCollection: TableCollection, name: ColumnName, description: Option[Description]) =>
      {
        val l = leftF.apply(tableCollection, name, description)
        fromKvpCoproduct(kvpSingleValueLeft.kvpTail).apply(l, name, description)
      }

  }
}
