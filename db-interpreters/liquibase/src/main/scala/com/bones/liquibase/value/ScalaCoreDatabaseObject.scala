package com.bones.liquibase.value

import com.bones.data.values.{BigDecimalData, BooleanData, ByteArrayData, DoubleData, EnumerationData, FloatData, IntData, LongData, ScalaCoreValue, ShortData, StringData}
import com.bones.liquibase.GenerateDatabaseObject.Name
import com.bones.liquibase.{DatabaseObjectValue, GenerateDatabaseObject}
import liquibase.change.ColumnConfig
import liquibase.structure.core.Table

trait ScalaCoreDatabaseObject extends DatabaseObjectValue[ScalaCoreValue]{

  val stringEncoding: String = "UTF-8"

  private def columnForType(columnType: String): Name => (List[(Table, GenerateDatabaseObject.TableItems)], List[GenerateDatabaseObject.TableItems]) =
    name => {
      val config = new ColumnConfig()
      config.setType(columnType)
      config.setEncoding(stringEncoding)
      asResult(config, name)
    }

  private val stringType = (name: String) => {
    val config = new ColumnConfig()
    config.setType("clob")
    config.setEncoding(stringEncoding)
    asResult(config, name)
  }

  override def databaseObject[A](alg: ScalaCoreValue[A]):
    Name => (List[(Table, GenerateDatabaseObject.TableItems)], List[GenerateDatabaseObject.TableItems]) = {
    alg match {
      case BooleanData(_) => columnForType("boolean")
      case IntData(_) => columnForType("int")
      case LongData(_) => columnForType("bigint")
      case ShortData(_) => columnForType("smallint")
      case FloatData(_) => columnForType("float")
      case DoubleData(_) => columnForType("double")
      case BigDecimalData(_) => columnForType("decimal")
      case ByteArrayData(_) => columnForType("blob")
      case EnumerationData(enumeration, validations) => stringType
      case StringData(_) => stringType
    }

  }
}
