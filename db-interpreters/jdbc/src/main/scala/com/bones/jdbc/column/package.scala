package com.bones.jdbc

import com.bones.data.values.DefaultValues
import com.bones.jdbc.column.DbColumnInterpreter.ColumnInterpreter
import com.bones.jdbc.column.DbColumnInterpreter.ColumnInterpreter.CNilColumnInterpreter

package object column {

  val defaultDbColumnInterpreter: ColumnInterpreter[DefaultValues] =
    (DefaultScalaCoreDbColumnInterpreter ++
      (DefaultColumnStringDbInterpreter ++
        (DefaultJavaTimeDbColumnInterpreter ++
          (DefaultJavaUtilDbColumnInterpreter ++ CNilColumnInterpreter))))

  object DefaultJavaTimeDbColumnInterpreter extends JavaTimeDbColumnInterpreter
  object DefaultJavaUtilDbColumnInterpreter extends JavaUtilDbColumnInterpreter
  object DefaultScalaCoreDbColumnInterpreter extends ScalaCoreDbColumnInterpreter
  object DefaultColumnStringDbInterpreter extends ColumnStringDbColumnInterpreter

}
