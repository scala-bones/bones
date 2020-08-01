package com.bones.jdbc.ideal

import com.bones.data.values.ScalaCoreValue

class ScalaCoreIdeal extends IdealValue[ScalaCoreValue] {
  override def columns[A](alg: ScalaCoreValue[A]): (TableCollection, ColumnName, Option[Description]) => TableCollection = ???
}
