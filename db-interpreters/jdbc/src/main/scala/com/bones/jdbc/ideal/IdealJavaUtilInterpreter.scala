package com.bones.jdbc.ideal

import com.bones.data.values.{BaseJavaUtilInterpreter, JavaUtilValue, UuidData}
import com.bones.jdbc.findUniqueConstraint
import com.bones.si.ideal.{IdealDataType, StringType}

object IdealJavaUtilInterpreter
    extends IdealValue[JavaUtilValue]
    with BaseJavaUtilInterpreter[IdealDataType] {
  override def columns[A](
    alg: JavaUtilValue[A]
  ): (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
    TableCollection,
    List[UniqueGroup]
  ) = {
    val uniqueConstraint = findUniqueConstraint(alg.validations)
    defaultColumns(matchJavaUtilValue(alg), uniqueConstraint)
  }

  override def uuidData(uuidData: UuidData): IdealDataType = StringType(37)
}
