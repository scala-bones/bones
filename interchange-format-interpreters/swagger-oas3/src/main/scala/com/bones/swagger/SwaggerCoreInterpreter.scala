package com.bones.swagger

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime, LocalTime}
import java.util.{Base64, UUID}

import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft, _}
import com.bones.syntax.NoAlgebra
import com.bones.validation.ValidationDefinition.{InvalidValue, ValidValue, ValidationOp, BigDecimalValidation => bdv, LongValidation => iv, StringValidation => sv}
import io.swagger.v3.oas.models.media._
import shapeless.{Coproduct, HList, Nat}

object SwaggerCoreInterpreter {

  type Name = String

  val exampleUuid = UUID.fromString("322dd565-0a28-4959-9b7e-42ba84149870")

  object SwaggerSchemas {
    def apply[S <: Schema[_]](mainSchema: S): SwaggerSchemas[S] =
      SwaggerSchemas(mainSchema, List.empty)
  }

  /** Responsible for keeping track of mainSchemas and referenceSchemas */
  case class SwaggerSchemas[+S <: Schema[_]](mainSchema: S,
                                             referenceSchemas: List[(String, Schema[_])])


  trait CustomSwaggerInterpreter[ALG[_]] {
    def toSchema[A](vd: ALG[A], description: Option[String], example: Option[A]): Name => SwaggerSchemas[Schema[_]]
  }

  val noAlgebraInterpreter = new CustomSwaggerInterpreter[NoAlgebra] {
    override def toSchema[A](vd: NoAlgebra[A], description: Option[String], example: Option[A]): Nothing =
      sys.error("Unreachable code")
  }

  def addBooleanSchema(name: String, description: Option[String], example: Option[Boolean], applyValidationDescription: Schema[_] => Schema[_]) = {
    val main = new BooleanSchema()
      .example(example.getOrElse(new java.lang.Boolean(true)))
      .description(description.getOrElse("value of type boolean"))
      .nullable(false)
      .name(name)
    val mainWithValidation = applyValidationDescription(main)
    SwaggerSchemas(mainWithValidation)
  }

  def addDateSchema(name: String, description: String, example: String, applyValidationDescription: Schema[_] => Schema[_]) = {
    val date = new DateSchema()
      .example(example)
      .nullable(false)
      .name(name)
      .description(description)
    val mainWithValidation = applyValidationDescription(date)
    SwaggerSchemas(mainWithValidation)
  }

  def addDateTimeSchema(name: String, description: String, example: String, applyValidationDescription: Schema[_] => Schema[_]) = {
    val date = new DateTimeSchema()
      .example(example)
      .nullable(false)
      .name(name)
      .description(description)
    val mainWithValidation = applyValidationDescription(date)
    SwaggerSchemas(mainWithValidation)
  }



  def addStringSchema(name: String, description: String, example: String, applyValidationDescription: Schema[_] => Schema[_]) = {
    val main = new StringSchema()
      .example(example)
      .nullable(false)
      .name(name)
      .description(description)
    val mainWithValidation = applyValidationDescription(main)
    SwaggerSchemas(mainWithValidation)
  }

  def addShortSchema(name: String, description: String, example: Short, applyValidationDescription: Schema[_] => Schema[_]) = {
    val main = new IntegerSchema()
      .nullable(false)
      .example(Short.box(example))
      .description(description)
      .format("int32")
      .maximum(java.math.BigDecimal.valueOf(Short.MaxValue.toLong))
      .minimum(java.math.BigDecimal.valueOf(Short.MinValue.toLong))
      .name(name)
    val mainWithValidation = applyValidationDescription(main)
    SwaggerSchemas(mainWithValidation)
  }

  def addIntSchema(name: String, description: String, example: Int, applyValidationDescription: Schema[_] => Schema[_]) = {
    val main = new IntegerSchema()
      .nullable(false)
      .example(Int.box(example))
      .description(description)
      .maximum(java.math.BigDecimal.valueOf(Int.MaxValue.toLong))
      .minimum(java.math.BigDecimal.valueOf(Int.MinValue.toLong))
      .format("int32")
      .name(name)
    val mainWithValidation = applyValidationDescription(main)
    SwaggerSchemas(mainWithValidation)
  }

  def addLongSchema(name: String, description: String, example: Long, applyValidationDescription: Schema[_] => Schema[_]) = {
    val intSchema = new IntegerSchema()
      .nullable(false)
      .example(Long.box(example))
      .description(description)
      .format("int64")
      .name(name)
    val withValidation = applyValidationDescription(intSchema)
    SwaggerSchemas(withValidation)
  }

  def addUuidSchema(name: String, description: String, example: UUID, applyValidationDescription: Schema[_] => Schema[_]) = {
    val uuidSchema = new UUIDSchema()
      .example(example)
      .description(description)
      .nullable(false)
      .name(name)
    val withValidation = applyValidationDescription(uuidSchema)
    SwaggerSchemas(withValidation)
  }

  def addNumberSchema(name: String, description: String, example: String, applyValidationDescription: Schema[_] => Schema[_]) = {
    val numberSchema = new NumberSchema()
      .example(example)
      .description(description)
      .nullable(false)
      .name(name)
    val withValidation = applyValidationDescription(numberSchema)
    SwaggerSchemas(withValidation)
  }

  def addBase64ByteArraySchema(name: String, description: String, example: Array[Byte], applyValidationDescription: Schema[_] => Schema[_]) = {
    val baSchema = new ByteArraySchema()
      .example(Base64.getEncoder.encodeToString(example))
      .description(description)
      .nullable(false)
      .name(name)
    val withValidation = applyValidationDescription(baSchema)
    SwaggerSchemas(withValidation)
  }

  def addEnumerationData(name: String, description: String, example: String, enumerations: List[String], applyValidationDescription: Schema[_] => Schema[_]) = {
    val stringSchema = new StringSchema()
    stringSchema
      .name(name)
      .nullable(false)
      .example(example)
      .description(description)
    enumerations.foreach(v => stringSchema.addEnumItemObject(v.toString))
    val withValidation = applyValidationDescription(stringSchema)
    SwaggerSchemas(withValidation)

  }
}

/**
  * Responsible for creating the data portion of the Swagger schema.
  */
trait SwaggerCoreInterpreter {

  import SwaggerCoreInterpreter._

  def localDateTimeFormatter: DateTimeFormatter

  def localDateFormatter: DateTimeFormatter

  def localTimeFormatter: DateTimeFormatter

  private def localDateExample = LocalDate.of(1970, 1, 1)

  private def localTimeExample = LocalTime.of(12, 0, 0, 0)

  private def localDateTimeExample = LocalDateTime.of(localDateExample, localTimeExample)

  import scala.collection.JavaConverters._

  private def copySchema(head: Schema[_], tail: ObjectSchema): ObjectSchema = {
    Option(head.getProperties).foreach(_.asScala.toList.foreach(prop =>
      tail.addProperties(prop._1, prop._2)))
    Option(head.getRequired).foreach(_.asScala.toList.foreach(req => tail.addRequiredItem(req)))
    tail
  }

  def fromSchemaWithAlg[ALG[_], A](
                                    gd: BonesSchema[ALG, A],
                                    customAlgebraInterpreter: CustomSwaggerInterpreter[ALG]
                                  ): Name => List[(Name, Schema[_])] = name => {
    val schemas = gd match {
      case x: HListConvert[ALG, _, _, A] =>
        valueDefinition(x, customAlgebraInterpreter, None, None)(name)
      case x: KvpCoproductConvert[ALG, _, A] =>
        valueDefinition(x, customAlgebraInterpreter, None, None)(name)
    }
    (name, schemas.mainSchema) :: schemas.referenceSchemas
  }

  def fromSchema[A](
                     gd: BonesSchema[NoAlgebra, A]
                   ): Name => List[(Name, Schema[_])] =
    fromSchemaWithAlg(gd, SwaggerCoreInterpreter.noAlgebraInterpreter)

  private def fromKvpCoproduct[ALG[_], C <: Coproduct](
                                                        co: KvpCoproduct[ALG, C],
                                                        customInterpreter: CustomSwaggerInterpreter[ALG],
                                                        description: Option[String]
                                                      ): SwaggerSchemas[ComposedSchema] =
    co match {
      case nil: KvpCoNil[_] => SwaggerSchemas(new ComposedSchema())
      case co: KvpSingleValueLeft[ALG, l, r]@unchecked => {
        val name = co.manifestL.runtimeClass.getSimpleName
        val lSchema = determineValueDefinition(co.kvpValue, customInterpreter, description, None)(name)
        val composedSchema = fromKvpCoproduct(co.kvpTail, customInterpreter, description)
        val objectSchema = new ObjectSchema().$ref(name)
        val mainSchema = composedSchema.mainSchema.addOneOfItem(objectSchema)
        SwaggerSchemas(
          mainSchema,
          (name, lSchema.mainSchema) :: lSchema.referenceSchemas ::: composedSchema.referenceSchemas)
      }
    }

  protected def fromKvpHList[ALG[_], H <: HList, HL <: Nat](
                                                             group: KvpHList[ALG, H, HL],
                                                             customInterpreter: CustomSwaggerInterpreter[ALG]): SwaggerSchemas[ObjectSchema] = {
    group match {
      case nil: KvpNil[_] =>
        val schema = new ObjectSchema()
        schema.nullable(false)
        SwaggerSchemas(schema)
      case op: KvpHListHead[ALG, H, al, h, hl, t, tl]@unchecked =>
        val headSchemas = fromKvpHList(op.head, customInterpreter)
        val tailSchemas = fromKvpHList(op.tail, customInterpreter)
        val schema = copySchema(headSchemas.mainSchema, tailSchemas.mainSchema)
        SwaggerSchemas(schema, headSchemas.referenceSchemas ::: tailSchemas.referenceSchemas)
      case op: KvpSingleValueHead[ALG, h, t, tl, o] =>
        val headSchemas =
          determineValueDefinition(
            op.fieldDefinition.op,
            customInterpreter,
            op.fieldDefinition.description,
            op.fieldDefinition.example
          )(op.fieldDefinition.key)
        val tailSchemas = fromKvpHList(op.tail, customInterpreter)
        tailSchemas.mainSchema.addProperties(op.fieldDefinition.key, headSchemas.mainSchema)
        val schema = copySchema(headSchemas.mainSchema, tailSchemas.mainSchema)
        SwaggerSchemas(schema, headSchemas.referenceSchemas ::: tailSchemas.referenceSchemas)
      case op: KvpConcreteTypeHead[ALG, a, ht, nt]@unchecked =>
        val headSchemas = op.bonesSchema match {
          case op: KvpCoproductConvert[ALG, c, a] =>
            fromKvpCoproduct(op.from, customInterpreter, None)
          case op: HListConvert[ALG, h, n, a] =>
            fromKvpHList(op.from, customInterpreter)
        }
        val tailSchemas = fromKvpHList(op.tail, customInterpreter)
        val schema = copySchema(headSchemas.mainSchema, tailSchemas.mainSchema)
        SwaggerSchemas(schema, headSchemas.referenceSchemas ::: tailSchemas.referenceSchemas)
    }
  }

  def determineValueDefinition[ALG[_], A](
                                           value: Either[KvpValue[A], ALG[A]],
                                           customInterpreter: CustomSwaggerInterpreter[ALG],
                                           description: Option[String],
                                           example: Option[A]
                                         ): Name => SwaggerSchemas[Schema[_]] =
    value match {
      case Left(kvp) => valueDefinition(kvp, customInterpreter, description, example)
      case Right(alg) => customInterpreter.toSchema(alg, description, example)
    }


  /**
    * Recursive method which builds up a Swagger Core Schema object from the DataClass definition.
    *
    * @param vd The DataClass definition to convert to a Schema
    **/
  def valueDefinition[ALG[_], A](
                                  vd: KvpValue[A],
                                  customInterpreter: CustomSwaggerInterpreter[ALG],
                                  description: Option[String],
                                  example: Option[A]): Name => SwaggerSchemas[Schema[_]] = {
    vd match {
      case op: OptionalKvpValueDefinition[ALG, b]@unchecked =>
        name =>
          val oasSchema = determineValueDefinition[ALG,b](op.valueDefinitionOp, customInterpreter, description, None)(name)
          oasSchema.copy(mainSchema = oasSchema.mainSchema.nullable(true))
      case bd: BooleanData =>
        name => addBooleanSchema(name, description, example, validations(bd.validations))
      case sd: StringData =>
        name => addStringSchema(name, description.getOrElse("value of type string"), example.getOrElse("ABC"), validations(sd.validations))
      case sd: ShortData =>
        name => {
          addShortSchema(name,
            description.getOrElse("value of type short"),
            example.asInstanceOf[Option[Short]].getOrElse((123: Short)),
            validations(sd.validations)
          )
        }
      case id: IntData =>
        name =>
          addIntSchema(
            name,
            description.getOrElse("value of type integer"),
            example.asInstanceOf[Option[Int]].getOrElse(123),
            validations(id.validations)
          )
      case ld: LongData =>
        name =>
          addLongSchema(
            name,
            description.getOrElse("value of type long"),
            example.asInstanceOf[Option[Long]].getOrElse(123l),
            validations(ld.validations)
          )
      case ud: UuidData =>
        name => addUuidSchema(name, description.getOrElse("value of type UUID"), example.getOrElse(exampleUuid), validations(ud.validations))
      case ldt: LocalDateTimeData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("value of type local date time"),
            localDateTimeFormatter.format(example.getOrElse(localDateTimeExample).asInstanceOf[LocalDateTime]),
            validations(ldt.validations)
          )
      case ld: LocalDateData =>
        name =>
          addDateSchema(
            name,
            description.getOrElse("value of type local date"),
            localDateFormatter.format(example.getOrElse(localDateExample).asInstanceOf[LocalDate]),
            validations(ld.validations)
          )
      case lt: LocalTimeData =>
        name =>
          addStringSchema(
            name,
            description.getOrElse("value of type local time"),
            localTimeFormatter.format(example.getOrElse(localTimeExample).asInstanceOf[LocalTime]),
            validations(lt.validations)
          )
      case dd: DoubleData =>
        name =>
          addNumberSchema(
            name,
            description.getOrElse("value of type double"),
            example.map(_.toString).getOrElse("3.14"),
            validations(dd.validations)
          )
      case fd: FloatData =>
        name => addNumberSchema(
          name,
          description.getOrElse("value of type float"),
          example.map(_.toString).getOrElse("3.14"),
          validations(fd.validations)
        )
      case bd: BigDecimalData =>
        name => addStringSchema(
          name,
          description.getOrElse("value fo type big decimal"),
          example.getOrElse(BigDecimal("3.14")).toString,
          validations(bd.validations)
        )
      case ba: ByteArrayData =>
        name =>
          addBase64ByteArraySchema(
            name,
            description.getOrElse("base64 encoded byte array"),
            example.getOrElse("0123456789abcdef".getBytes).asInstanceOf[Array[Byte]],
            validations(ba.validations)
          )
      case ld: ListData[ALG, t]@unchecked =>
        name =>
          val itemSchema = determineValueDefinition(ld.tDefinition, customInterpreter, description, None)(name)
          val arraySchema = new ArraySchema()
          arraySchema.setItems(itemSchema.mainSchema)
          arraySchema.description(description.getOrElse("value of type list"))
          arraySchema.name(name)
          val withValidation = validations(ld.validations)(arraySchema)
          SwaggerSchemas(withValidation, itemSchema.referenceSchemas)
      case ed: EitherData[ALG, a, b]@unchecked =>
        name =>
          val a =
            determineValueDefinition[ALG,a](ed.definitionA, customInterpreter, description, None)("left" + name.capitalize)
          val b =
            determineValueDefinition[ALG,b](ed.definitionB, customInterpreter, description, None)("right" + name.capitalize)
          val composedSchema = new ComposedSchema()
            .addAnyOfItem(a.mainSchema)
            .addAnyOfItem(b.mainSchema)
            .nullable(false)
            .description(description.getOrElse("value of type either"))
            .name(name)
          SwaggerSchemas(composedSchema, a.referenceSchemas ::: b.referenceSchemas)
      case esd: EnumerationData[e, a]@unchecked =>
        name =>
          addEnumerationData(
            name.toString,
            description.getOrElse(s"enumeration of type ${esd.manifestOfA.getClass.getSimpleName}"),
            example.orElse(esd.enumeration.values.headOption).map(_.toString).getOrElse(""),
            esd.enumeration.values.toList.map(_.toString),
            validations(esd.validations)
          )
      case gd: KvpHListValue[ALG, h, hl]@unchecked =>
        name =>
          val schemas = fromKvpHList(gd.kvpHList, customInterpreter)
          schemas.mainSchema.name(name).description(description.getOrElse("value of type list"))
          validations(gd.validations)(schemas.mainSchema)
          schemas
      case x: HListConvert[ALG, _, _, a]@unchecked =>
        name =>
          val schemas = fromKvpHList(x.from, customInterpreter)
          schemas.mainSchema.name(name).description(description.getOrElse("value of type object"))
          validations(x.validations)(schemas.mainSchema)
          schemas
      case co: KvpCoproductConvert[ALG, c, a]@unchecked =>
        name =>
          val coproductSchemas = fromKvpCoproduct(co.from, customInterpreter, description)
          val main = new ObjectSchema().$ref(name)
          validations(co.validations)(main)
          SwaggerSchemas(
            main,
            (name, coproductSchemas.mainSchema) :: coproductSchemas.referenceSchemas)
      case co: KvpCoproductValue[ALG, c]@unchecked =>
        name =>
          val coproductSchemas = fromKvpCoproduct(co.kvpCoproduct, customInterpreter, description)
          val main = new ObjectSchema()
            .name(name)
            .$ref(name)
          SwaggerSchemas(
            main,
            (name, coproductSchemas.mainSchema) :: coproductSchemas.referenceSchemas)

    }
  }

  def validations[A](list: List[ValidationOp[A]]): Schema[_] => Schema[_] = schema => {
    list.map(v => validation(v).apply(schema))
    schema
  }

  /**
    * Responsible for adding validation specific details to the SwaggerCore files.
    *
    * @param op The validation from we will add to the
    * @tparam A The type which is to be validated.
    * @return
    */
  def validation[A](op: ValidationOp[A]): Schema[_] => Schema[_] = {
    op match {
      case ValidValue(values) =>
        schema => {
          val typedSchema = schema.asInstanceOf[Schema[A]]
          values.foreach(typedSchema.addEnumItemObject)
          typedSchema
        }
      case InvalidValue(values) =>
        val invalidDescription =
          s"These values are not allowed: ${values.mkString("('", ",", "')")}."
        schema => {
          val description = schema.getDescription
          val newDescription =
            if (description.isEmpty) invalidDescription
            else description + " " + invalidDescription
          schema.setDescription(newDescription)
          schema
        }
      case sv.IsAlphanumeric =>
        schema =>
          schema.pattern("^[:alnum:]+$"); schema
      case sv.MinLength(min) =>
        schema =>
          schema.minLength(min)
      case sv.MaxLength(max) =>
        schema =>
          schema.maxLength(max)
      case sv.MatchesRegex(r) =>
        schema =>
          schema.pattern(r.toString())
      case sv.Length(l) =>
        schema =>
          schema.minLength(l).maxLength(l)
      case sv.Custom(_, _, _) => identity
      case sv.Uppercase => identity
      case sv.Token => identity
      case sv.Lowercase => identity

      case iv.Between(min, max) =>
        _.exclusiveMinimum(true)
          .exclusiveMaximum(true)
          .minimum(new java.math.BigDecimal(min))
          .maximum(new java.math.BigDecimal(max))
      case iv.Greater(gt) =>
        _.minimum(new java.math.BigDecimal(gt)).exclusiveMinimum(true)
      case iv.Less(lt) =>
        _.maximum(new java.math.BigDecimal(lt)).exclusiveMaximum(true)
      case iv.Max(max) => _.maximum(new java.math.BigDecimal(max))
      case iv.Min(min) => _.minimum(new java.math.BigDecimal(min))
      case iv.Multiple(x) => _.multipleOf(new java.math.BigDecimal(x))
      case iv.Negative =>
        _.exclusiveMaximum(true).maximum(java.math.BigDecimal.ZERO)
      case iv.Positive =>
        _.exclusiveMinimum(true).minimum(java.math.BigDecimal.ZERO)
      case bdv.Max(max) => _.maximum(max.bigDecimal)
      case bdv.Min(min) => _.minimum(min.bigDecimal)

      case _ => identity

    }
  }

}
