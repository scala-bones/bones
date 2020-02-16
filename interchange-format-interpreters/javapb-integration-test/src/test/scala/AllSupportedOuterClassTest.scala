import java.io.{ByteArrayOutputStream, IOException}
import java.math.BigDecimal
import java.nio.charset.Charset
import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import java.util.{Base64, UUID}

import com.bones.protobuf.ProtobufUtcSequentialEncoderAndValidator
import com.bones.schemas.Schemas
import com.bones.schemas.Schemas.Currency
import com.google.protobuf.ByteString
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.must.Matchers

class AllSupportedOuterClassTest extends AnyFunSuite with Matchers {

  val encode = ProtobufUtcSequentialEncoderAndValidator.encodeToBytes(Schemas.allSupportCaseClass)
  val decode = ProtobufUtcSequentialEncoderAndValidator.fromBytes(Schemas.allSupportCaseClass)



  test("integration test") {
    val build = AllSupportedOuterClass.AllSupported.newBuilder
    build.setBoolean(true)
    build.setInt(3455)
    build.setLong(8848884l)
    build.addListOfInt(5)
    build.addListOfInt(6)
    build.setString("A String")
    build.setFloat(1.234f)
    build.setShort(44)
    build.setDouble(1234)
    val bs = ByteString.copyFromUtf8("The quick brown fox")
    build.setByteArray(bs)
    val epochDay = LocalDate.of(2018, 1, 1)
    build.setLocalDate(epochDay.toEpochDay)

    val timestamp = AllSupportedOuterClass.AllSupported.Timestamp.newBuilder
    val time = LocalDateTime.now
    timestamp.setSeconds(time.toEpochSecond(ZoneOffset.UTC))
    timestamp.setNanos(time.getNano)

    build.setLocalDateTime(timestamp)
    val uuid = UUID.randomUUID
    build.setUuid(uuid.toString)
    build.setEnumeration("USD")
    build.setBigDecimal(new BigDecimal("1.234").toString)
    val child = AllSupportedOuterClass.AllSupported.Child.newBuilder
    child.setBoolean(false)
    child.addListOfInt(42)
    child.setDouble(55555)
    build.setChild(child)

    build.setEitherFieldRight(77)

    val compactDisc = AllSupportedOuterClass.AllSupported.CompactDisc.newBuilder()
    compactDisc.setName("Duchess Says")
    compactDisc.setCaseQuality("Excellent")
    compactDisc.setCdQuality("Mint")

    build.setCompactDisc(compactDisc)

    build.setInt2(2)
    val output = new ByteArrayOutputStream

    try build.build.writeTo(output)
    catch {
      case e: IOException =>
        e.printStackTrace()
        fail
    }

    val encoded = output.toByteArray

    val base64 = Base64.getEncoder().encode(encoded)
//    println(s"BASE:${new String(base64)}")

    decode(output.toByteArray) match {
      case Left(err) => fail(err.toString())
      case Right(all) => {
        all.b mustBe true
        all.i mustBe 3455
        all.l mustBe 8848884l
        all.ls mustBe List(5,6)
        all.str mustBe "A String"
        all.f mustBe 1.234f
        all.s mustBe 44
        all.d mustBe 1234
        new String(all.ba, Charset.forName("UTF8")) mustBe "The quick brown fox"
        all.ld mustBe epochDay
        all.ldt mustBe time
        all.uuid mustBe uuid
        all.currency mustBe Currency.USD
        all.bd.compare(new BigDecimal("1.234")) mustBe 0
        all.int2 mustBe 2
        all.either mustBe Right(77)

        val child = all.child
        child.b mustBe Some(false)
        child.ls mustBe Some(List(42))
        child.d mustBe Some(55555)
        child.i mustBe None


        val bonesEncoded = encode(all)

        val javaAllSupported = AllSupportedOuterClass.AllSupported.parseFrom(bonesEncoded)


        javaAllSupported mustBe build.build()

      }



    }



  }
}