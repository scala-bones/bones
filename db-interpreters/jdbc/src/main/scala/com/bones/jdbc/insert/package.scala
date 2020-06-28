package com.bones.jdbc

import com.bones.data.values.{DefaultValues, CNilF}
import com.bones.jdbc.insert.DbInsert.InsertPair
import shapeless.{:+:, Coproduct, Inl, Inr}

package object insert {

  val defaultDbInsertInterpreter: DbInsertValue[DefaultValues] =
    DefaultScalaCoreDbInsert ++
      (DefaultCustomStringDbInsert ++
        (DefaultJavaTimeDbInsert ++
          (DefaultJavaUtilDbInsert ++ CNilUpdateInterpreter)))

  object DefaultScalaCoreDbInsert extends ScalaCoreDbInsert
  object DefaultJavaUtilDbInsert extends JavaUtilDbInsert
  object DefaultJavaTimeDbInsert extends JavaTimeDbInsert
  object DefaultCustomStringDbInsert extends CustomStringDbInsert


}
