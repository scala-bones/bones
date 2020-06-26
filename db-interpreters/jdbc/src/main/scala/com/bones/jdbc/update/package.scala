package com.bones.jdbc

import com.bones.data.custom.AllCustomAlgebras
import com.bones.jdbc.update.DbUpdateValues.CustomDbUpdateInterpreter
import com.bones.jdbc.update.DbUpdateValues.CustomDbUpdateInterpreter.CNilUpdateInterpreter

package object update {

  val defaultDbUpdateInterpreter: CustomDbUpdateInterpreter[AllCustomAlgebras] =
    DefaultScalaCoreDbUpdate ++
      (DefaultCustomStringDbUpdate ++
        (DefaultJavaTimeDbUpdate ++
          (DefaultJavaUtilDbUpdate ++ CNilUpdateInterpreter)))

  object DefaultJavaTimeDbUpdate extends JavaTimeDbUpdate
  object DefaultJavaUtilDbUpdate extends JavaUtilDbUpdate
  object DefaultScalaCoreDbUpdate extends ScalaCoreDbUpdate
  object DefaultCustomStringDbUpdate extends CustomStringDbUpdate
}
