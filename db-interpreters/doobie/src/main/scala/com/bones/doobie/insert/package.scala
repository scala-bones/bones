package com.bones.doobie

import com.bones.data.values.DefaultValues
import com.bones.jdbc.insert.DbInsert

package object insert {
  val defaultInsertInterpreter: InsertInterpreter[DefaultValues] =
    new InsertInterpreter[DefaultValues] {
      override val insert: DbInsert[DefaultValues] =
        com.bones.jdbc.insert.defaultDbInsertInterpreter
    }
}
