package com.bones.jdbc

import java.util.Properties

import com.bones.schemas.Schemas
import org.scalatest.FunSuite

class DbGetTest extends FunSuite {

  import java.sql.Connection
  import java.sql.DriverManager

  val url = "jdbc:postgresql://localhost/tstevens"
  val props = new Properties()
  val conn: Connection = DriverManager.getConnection(url)

  val result = DbGet.getEntityWithConnection(Schemas.creditCardSchema)(2)(conn)

  println(result)

//  Left(
//    NonEmptyList(
//      RequiredData(List(deletedAt),DateTimeData((ParseCaseSensitive(false)(Value(Year,4,10,EXCEEDS_PAD)'-'Value(MonthOfYear,2)'-'Value(DayOfMonth,2))'T'(Value(HourOfDay,2)':'Value(MinuteOfHour,2)[':'Value(SecondOfMinute,2)[Fraction(NanoOfSecond,0,9,DecimalPoint)]]))[Offset(+HH:MM:ss,'Z')['['ParseCaseSensitive(true)ZoneRegionId()']']],ISO date-time format with the offset and zone if available, such as '2011-12-03T10:15:30', '2011-12-03T10:15:30+01:00' or '2011-12-03T10:15:30+01:00[Europe/Paris]',List())),
//      RequiredData(List(countryIso, billingLocation),StringData(List(ValidValue(Vector(US, CA, MX))))),
//      RequiredData(List(zipCode, billingLocation),StringData(List())))
//  )

}
