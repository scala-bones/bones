package com.bones.jdbc

import com.bones.data.values.DefaultValues
import com.bones.jdbc.column.ColumnValue.CNilColumnValue

package object column {

  val defaultDbColumnInterpreter: ColumnValue[DefaultValues] =
    (DefaultScalaCoreDbColumnValue ++
      (DefaultColumnStringDbValue ++
        (DefaultJavaTimeDbColumnValue ++
          (DefaultJavaUtilDbColumnValue ++ CNilColumnValue))))

  object DefaultJavaTimeDbColumnValue extends JavaTimeDbColumnValue
  object DefaultJavaUtilDbColumnValue extends JavaUtilDbColumnValue
  object DefaultScalaCoreDbColumnValue extends ScalaCoreDbColumnValue
  object DefaultColumnStringDbValue extends ColumnStringDbColumnValue

}
