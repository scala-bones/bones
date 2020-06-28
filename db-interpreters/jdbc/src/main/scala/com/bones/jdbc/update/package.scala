package com.bones.jdbc

import com.bones.data.values.DefaultValues
import com.bones.jdbc.update.DbUpdateValue.CNilUpdateInterpreter


package object update {

  val defaultDbUpdateInterpreter: DbUpdateValue[DefaultValues] =
    DefaultScalaCoreDbUpdate ++
      (DefaultCustomStringDbUpdate ++
        (DefaultJavaTimeDbUpdate ++
          (DefaultJavaUtilDbUpdate ++ CNilUpdateInterpreter)))

  object DefaultJavaTimeDbUpdate extends JavaTimeDbUpdate
  object DefaultJavaUtilDbUpdate extends JavaUtilDbUpdate
  object DefaultScalaCoreDbUpdate extends ScalaCoreDbUpdate
  object DefaultCustomStringDbUpdate extends CustomStringDbUpdate
}
