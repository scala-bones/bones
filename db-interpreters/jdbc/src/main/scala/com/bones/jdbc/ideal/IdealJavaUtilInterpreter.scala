package com.bones.jdbc.ideal

import com.bones.data.values.{BaseJavaUtilInterpreter, JavaUtilValue, UuidData}
import com.bones.si.ideal.{IdealColumn, IdealDataType, StringType}

object IdealJavaUtilInterpreter extends IdealValue[JavaUtilValue] with BaseJavaUtilInterpreter[IdealDataType] {
  override def columns[A](alg: JavaUtilValue[A]): (TableCollection, ColumnName, Option[Description]) => TableCollection = {
    (tableCollection, name, description) =>
    {
      val newType = matchJavaUtilValue(alg)
      val newColumn = IdealColumn(name, newType, false, description)
      tableCollection.prependColumn(newColumn)
    }
  }


  override def uuidData(uuidData: UuidData): IdealDataType = StringType(37)
}
