package com.bones.jdbc

import java.sql.{CallableStatement, Connection, SQLException}

import cats.data.NonEmptyList
import com.bones.data.Error.{ExtractionError, SystemError}
import com.bones.syntax.{long, lv}
import javax.sql.DataSource

import scala.annotation.tailrec
import scala.util.control.NonFatal

object DbUtil {

  /**
    * Converts from camelCase to snake_case
    * e.g.: camelCase => camel_case
    *
    * @param name the camelCase name to convert
    * @return snake_case version of the string passed
    */
  def camelToSnake(name: String): String = {
    @tailrec
    def go(accDone: List[Char], acc: List[Char]): List[Char] = acc match {
      case Nil => accDone
      case a :: b :: c :: tail if a.isUpper && b.isUpper && c.isLower =>
        go(accDone ++ List(a, '_', b, c), tail)
      case a :: b :: tail if a.isLower && b.isUpper =>
        go(accDone ++ List(a, '_', b), tail)
      case a :: tail => go(accDone :+ a, tail)
    }
    go(Nil, name.toList).mkString.toLowerCase
  }

  def withDataSource[A](ds: DataSource)(f: Connection => Either[NonEmptyList[ExtractionError], A])
    : Either[NonEmptyList[ExtractionError], A] =
    try {
      val con = ds.getConnection
      val result = f(con)
      con.close()
      result
    } catch {
      case ex: SQLException =>
        Left(NonEmptyList.one(SystemError(List.empty, ex, None)))
    }

  def withStatement[A](con: CallableStatement)(
    f: CallableStatement => Either[NonEmptyList[ExtractionError], A])
    : Either[NonEmptyList[ExtractionError], A] =
    try {
      f(con)
    } catch {
      case NonFatal(th) =>
        Left(NonEmptyList.one(SystemError(th, Some("Error executing CallableStatement"))))
    } finally {
      con.close()
    }

  val longIdKeyValueDef = ("id", long(lv.min(0)))
}
