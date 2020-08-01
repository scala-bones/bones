package com.bones.jdbc.ideal

import com.bones.data.{
  KvpCoNil,
  KvpCollection,
  KvpCollectionTemplate,
  KvpCoproductTemplate,
  KvpSingleValueLeft
}
import com.bones.data.values.InvalidStructureError
import shapeless.Coproduct

trait IdealCoproductInterpreter[ALG[_]]
    extends KvpCoproductTemplate[
      ALG,
      (
        TableCollection,
        Option[ColumnName],
        Option[Description]) => Either[InvalidStructureError, TableCollection]] {

  val algInterpreter: IdealValue[ALG]
  def fromCollection[A: Manifest](kvpCollection: KvpCollection[ALG, A]):
    (TableCollection, Option[ColumnName], Option[Description]) => Either[InvalidStructureError, TableCollection]

  override def kvpCoNil(kvpCoproduct: KvpCoNil[ALG]): (
    TableCollection,
    Option[ColumnName],
    Option[Description]) => Either[InvalidStructureError, TableCollection] =
    (tc, _, _) => Right(tc)

  override def kvpSingleValueLeft[A, R <: Coproduct](
    kvpSingleValueLeft: KvpSingleValueLeft[ALG, A, R]): (
    TableCollection,
    Option[ColumnName],
    Option[Description]) => Either[InvalidStructureError, TableCollection] = {
    val leftF = kvpSingleValueLeft.kvpValue match {
      case Left(kvpCollection: KvpCollection[ALG, A]) =>
        implicit val manifestOfA = kvpCollection.manifestOfA
        fromCollection[A](kvpCollection)
      case Right(value) =>
        (
          tableCollection: TableCollection,
          name: Option[ColumnName],
          description: Option[Description]) =>
          name match {
            case Some(n) =>
              Right(algInterpreter.columns(value).apply(tableCollection, n, description))
            case None => Left(InvalidStructureError("Expecting a name for this coproduct type"))
          }
    }

    (
      tableCollection: TableCollection,
      name: Option[ColumnName],
      description: Option[Description]) => {

      for {
        l <- leftF(tableCollection, name, description)
        t <- fromKvpCoproduct(kvpSingleValueLeft.kvpTail).apply(l, name, description)
      } yield t

    }

  }
}
