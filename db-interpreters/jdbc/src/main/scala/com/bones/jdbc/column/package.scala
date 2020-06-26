package com.bones.jdbc

import com.bones.data.custom.AllCustomAlgebras
import com.bones.jdbc.column.DbColumnInterpreter.ColumnInterpreter
import com.bones.jdbc.column.DbColumnInterpreter.ColumnInterpreter.CNilColumnInterpreter

package object column {

  val defaultDbColumnInterpreter: ColumnInterpreter[AllCustomAlgebras] =
    (DefaultScalaCoreDbColumnInterpreter ++
      (DefaultColumnStringDbInterpreter ++
        (DefaultJavaTimeDbColumnInterpreter ++
          (DefaultJavaUtilDbColumnInterpreter ++ CNilColumnInterpreter))))

  object DefaultJavaTimeDbColumnInterpreter extends JavaTimeDbColumnInterpreter
  object DefaultJavaUtilDbColumnInterpreter extends JavaUtilDbColumnInterpreter
  object DefaultScalaCoreDbColumnInterpreter extends ScalaCoreDbColumnInterpreter
  object DefaultColumnStringDbInterpreter extends ColumnStringDbColumnInterpreter

}
