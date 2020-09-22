package com.bones.doobie

import com.bones.data.values.DefaultValues
import com.bones.jdbc

package object select {
  val defaultSelectInterpreter: SelectInterpreter[DefaultValues] =
    new SelectInterpreter[DefaultValues] {
      override val jdbcSelectInterpreter: jdbc.select.SelectInterpreter[DefaultValues] =
        com.bones.jdbc.select.defaultSelectInterpreter
    }
}
