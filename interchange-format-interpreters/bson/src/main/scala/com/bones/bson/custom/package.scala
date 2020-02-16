package com.bones.bson

import java.time.format.DateTimeFormatter

package object custom {
  /** Encoder/Validator which uses default ISO format. */
  object BsoJavaTimeEncoder
    extends BsonJavaTimeEncoder
      with BsonJavaTimeValidator {

    override val offsetDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_DATE_TIME
    override val offsetTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_OFFSET_TIME
    override val zonedDateTimeFormatter: DateTimeFormatter = DateTimeFormatter.ISO_ZONED_DATE_TIME

  }

}
