package com.bones.oas3

import java.util.UUID

import com.bones.data.{KvpCoNil, KvpCoproduct, KvpSingleValueLeft}
import com.bones.data._
import com.bones.validation.ValidationDefinition.{InvalidValue, ValidValue, ValidationOp, BigDecimalValidation => bdv, LongValidation => iv, StringValidation => sv}
import io.swagger.v3.oas.models.media._
import shapeless.{Coproduct, HList, Nat}

object SwaggerCoreInterpreter {


  type Name = String
  object SwaggerSchemas {
    def apply[S <: Schema[_]](mainSchema: S): SwaggerSchemas[S] = SwaggerSchemas(mainSchema, List.empty)
  }
  case class SwaggerSchemas[+S <: Schema[_]](mainSchema: S, referenceSchemas: List[(String, Schema[_])])


  /**
    * An implementation of the SwaggerCoreInterpreter using
    * ISO date as the default date interpreter.
    */
  def isoInterpreter: SwaggerCoreInterpreter = new SwaggerCoreInterpreter {
    override def dateFormatterExample: String = "'2011-12-03'"

    override def localDateFormatterExample: String = "2011-12-03T10:15:30"
  }

  def apply[A](gd: BonesSchema[A]): Name => List[(Name, Schema[_])] = isoInterpreter.apply(gd)
}

/**
  * Responsible for creating the data portion of the Swagger schema.
  */
trait SwaggerCoreInterpreter {

  import SwaggerCoreInterpreter.{SwaggerSchemas, Name}

  def dateFormatterExample: String
  def localDateFormatterExample: String

  import scala.collection.JavaConverters._
  private def copySchema(head: Schema[_], tail: ObjectSchema): ObjectSchema = {
    Option(head.getProperties).foreach(_.asScala.toList.foreach(prop => tail.addProperties(prop._1, prop._2)))
    Option(head.getRequired).foreach(_.asScala.toList.foreach(req => tail.addRequiredItem(req)))
    tail
  }

  def apply[A](gd: BonesSchema[A]): Name => List[(Name, Schema[_])] = name => {
    val schemas = gd match {
      case x: HListConvert[_, _, A] => fromValueDef(x)(name)
      case x: KvpCoproductConvert[_, A] => fromValueDef(x)(name)
    }
    (name, schemas.mainSchema) :: schemas.referenceSchemas
  }

  private def fromKvpCoproduct[C<:Coproduct](co: KvpCoproduct[C]): SwaggerSchemas[ComposedSchema] = {
    co match {
      case KvpCoNil => SwaggerSchemas(new ComposedSchema())
      case co: KvpSingleValueLeft[l,r] => {
        val name = co.manifestL.runtimeClass.getSimpleName
        val lSchema = fromValueDef(co.kvpValue)(name)
        val composedSchema = fromKvpCoproduct(co.kvpTail)
        val objectSchema = new ObjectSchema().$ref(name)
        val mainSchema = composedSchema.mainSchema.addOneOfItem(objectSchema)
        SwaggerSchemas(mainSchema, (name, lSchema.mainSchema) :: lSchema.referenceSchemas ::: composedSchema.referenceSchemas)
      }
    }
  }

  private def fromKvpHList[H <: HList, HL <: Nat](
      group: KvpHList[H, HL]):  SwaggerSchemas[ObjectSchema] = {
    group match {
      case KvpNil =>
        val schema = new ObjectSchema()
        schema.nullable(false)
        SwaggerSchemas(schema)
      case op: KvpHListHead[H, al, h, hl, t, tl] =>
        val headSchemas = fromKvpHList(op.head)
        val tailSchemas = fromKvpHList(op.tail)
        val schema = copySchema(headSchemas.mainSchema, tailSchemas.mainSchema)
        SwaggerSchemas(schema, headSchemas.referenceSchemas ::: tailSchemas.referenceSchemas)
      case op: KvpSingleValueHead[h, t, tl, o] =>
        val headSchemas = fromValueDef(op.fieldDefinition.op)(op.fieldDefinition.key)
        val tailSchemas = fromKvpHList(op.tail)
        tailSchemas.mainSchema.addProperties(op.fieldDefinition.key, headSchemas.mainSchema)
        val schema = copySchema(headSchemas.mainSchema, tailSchemas.mainSchema)
        SwaggerSchemas(schema, headSchemas.referenceSchemas ::: tailSchemas.referenceSchemas)
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xh, xl] =>
        val name = op.manifestOfA.runtimeClass.getSimpleName
        val headSchemas = fromValueDef(op.hListConvert)(name)
        val tailSchemas = fromKvpHList(op.tail)
        val schema = copySchema(headSchemas.mainSchema, tailSchemas.mainSchema)
        SwaggerSchemas(schema, headSchemas.referenceSchemas ::: tailSchemas.referenceSchemas)
    }
  }

  /**
    * Recursive method which builds up a Swagger Core Schema object from the DataClass definition.
    * @param vd The DataClass definition to convert to a Schema
    **/
  def fromValueDef[A](vd: KvpValue[A]):  Name => SwaggerSchemas[Schema[_]] = {
    vd match {
      case op: OptionalKvpValueDefinition[b] =>
        name =>
          val oasSchema = fromValueDef(op.valueDefinitionOp)(name)
          oasSchema.copy(mainSchema = oasSchema.mainSchema.nullable(true))
      case _: BooleanData =>
        name =>
          val main = new BooleanSchema()
            .example(new java.lang.Boolean(true))
            .nullable(false)
            .name(name)
          SwaggerSchemas(main)

      case _: StringData =>
        name =>
          val main = new StringSchema()
            .example("ABC")
            .nullable(false)
            .name(name)
          SwaggerSchemas(main)
      case ShortData(_) =>
        name =>
          val main = new IntegerSchema()
            .nullable(false)
            .example(123)
            .format("int32")
            .maximum(java.math.BigDecimal.valueOf(Short.MaxValue.toLong))
            .minimum(java.math.BigDecimal.valueOf(Short.MinValue.toLong))
            .name(name)
          SwaggerSchemas(main)
      case IntData(_) =>
        name =>
          val main = new IntegerSchema()
            .nullable(false)
            .example(123)
            .maximum(java.math.BigDecimal.valueOf(Int.MaxValue.toLong))
            .minimum(java.math.BigDecimal.valueOf(Int.MinValue.toLong))
            .format("int32")
            .name(name)
          SwaggerSchemas(main)
      case _: LongData =>
        name =>
          val intSchema = new IntegerSchema()
            .nullable(false)
            .example(123)
            .format("int64")
            .name(name)
          SwaggerSchemas(intSchema)
      case _: UuidData =>
        name =>
          val uuidSchema = new UUIDSchema()
            .example(UUID.randomUUID().toString)
            .nullable(false)
            .name(name)
          SwaggerSchemas(uuidSchema)
      case _: LocalDateTimeData =>
        name =>
          val stringSchema = new StringSchema()
            .example(dateFormatterExample)
            .nullable(false)
            .name(name)
          SwaggerSchemas(stringSchema)
      case _: LocalDateData =>
        name =>
          val stringSchema = new StringSchema()
            .example(dateFormatterExample)
            .nullable(false)
            .name(name)
          SwaggerSchemas(stringSchema)
      case DoubleData(_) =>
        name =>
          val numberSchema = new NumberSchema()
            .example("3.14")
            .nullable(false)
            .name(name)
          SwaggerSchemas(numberSchema)
      case FloatData(_) =>
        name =>
          val numberSchema = new NumberSchema()
            .example("3.14")
            .nullable(false)
          SwaggerSchemas(numberSchema)
      case _: BigDecimalData =>
        name =>
          val stringSchema = new StringSchema()
            .example("3.14")
            .nullable(false)
            .name(name)
          SwaggerSchemas(stringSchema)
      case _: ByteArrayData =>
        name =>
          val baSchema = new ByteArraySchema()
            .example("0123456789abcdef")
            .nullable(false)
            .name(name)
          SwaggerSchemas(baSchema)
      case ListData(definition, _) =>
        name =>
          val itemSchema = fromValueDef(definition)(name)
          val arraySchema = new ArraySchema()
          arraySchema.setItems(itemSchema.mainSchema)
          arraySchema.name(name)
          SwaggerSchemas(arraySchema, itemSchema.referenceSchemas)
      case ed: EitherData[a, b] =>
        name =>
          val a = fromValueDef(ed.definitionA)("left" + name.capitalize)
          val b = fromValueDef(ed.definitionB)("right" + name.capitalize)
          val composedSchema = new ComposedSchema()
            .addAnyOfItem(a.mainSchema)
            .addAnyOfItem(b.mainSchema)
            .nullable(false)
            .name(name)
          SwaggerSchemas(composedSchema, a.referenceSchemas ::: b.referenceSchemas)
      case esd: EnumerationData[e,a] =>
        name =>
          val stringSchema = new StringSchema()
          stringSchema.name(name)
              .nullable(false)
              .example(esd.enumeration.values.head.toString)
          esd.enumeration.values.foreach(v =>
            stringSchema.addEnumItemObject(v.toString))
          SwaggerSchemas(stringSchema)
      case gd: KvpHListValue[h, hl] =>
        name =>
          val schemas = fromKvpHList(gd.kvpHList)
          schemas.mainSchema.name(name)
          schemas
      case x: HListConvert[_, _, a] =>
        name =>
          val schemas = fromKvpHList(x.from)
          schemas.mainSchema.name(name)
          schemas
      case co: KvpCoproductConvert[c,a] =>
        name =>
          val coproductSchemas = fromKvpCoproduct(co.from)
          val main = new ObjectSchema().$ref(name)
          SwaggerSchemas(main, (name, coproductSchemas.mainSchema) :: coproductSchemas.referenceSchemas)
      case co: KvpCoproductValue[c] =>
        name =>
          val coproductSchemas = fromKvpCoproduct(co.kvpCoproduct)
          val main = new ObjectSchema()
            .name(name)
            .$ref(name)
          SwaggerSchemas(main, (name, coproductSchemas.mainSchema) :: coproductSchemas.referenceSchemas)

    }
  }

  /**
    * Responsible for adding validation specific details to the SwaggerCore files.
    * @param op The validation from we will add to the
    * @tparam A The type which is to be validated.
    * @return
    */
  def validation[A](op: ValidationOp[A]): Schema[_] => Schema[_] = {
    op match {
      case ValidValue(values) =>
        schema =>
          {
            val typedSchema = schema.asInstanceOf[Schema[A]]
            values.foreach(typedSchema.addEnumItemObject)
            typedSchema
          }
      case InvalidValue(values) =>
        val invalidDescription =
          s"These values are not allowed: ${values.mkString("('", ",", "')")}."
        schema =>
          {
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
      case sv.Guid =>
        schema =>
          schema
            .minLength(36)
            .maxLength(36)
            .pattern(
              "(^([0-9A-Fa-typeToConversion]{8}[-][0-9A-Fa-typeToConversion]{4}[-][0-9A-Fa-typeToConversion]{4}[-][0-9A-Fa-typeToConversion]{4}[-][0-9A-Fa-typeToConversion]{12})$)")
            .format("guid")
      case sv.Uppercase  => identity
      case sv.CreditCard => identity
      case sv.Token      => identity
      case sv.Email =>
        val emailSchema = new EmailSchema()
        schema =>
          emailSchema.setName(schema.getName); emailSchema
      case sv.Hex       => identity
      case sv.Base64    => identity
      case sv.Hostname  => _.format("hostname")
      case sv.Ipv4      => _.format("ipv4")
      case sv.Lowercase => identity
      case sv.Uri       => _.format("uri")

      case iv.Between(min, max) =>
        _.exclusiveMinimum(true)
          .exclusiveMaximum(true)
          .minimum(new java.math.BigDecimal(min))
          .maximum(new java.math.BigDecimal(max))
      case iv.Greater(gt) =>
        _.minimum(new java.math.BigDecimal(gt)).exclusiveMinimum(true)
      case iv.Less(lt) =>
        _.maximum(new java.math.BigDecimal(lt)).exclusiveMaximum(true)
      case iv.Max(max)    => _.maximum(new java.math.BigDecimal(max))
      case iv.Min(min)    => _.minimum(new java.math.BigDecimal(min))
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
