package com.bones.oas3

import java.util.UUID

import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.{
  InvalidValue,
  ValidValue,
  ValidationOp,
  BigDecimalValidation => bdv,
  LongValidation => iv,
  StringValidation => sv
}
import io.swagger.v3.oas.models.media._
import shapeless.{HList, Nat}

object SwaggerCoreInterpreter {

  /**
    * An implementation of the SwaggerCoreInterpreter using
    * ISO date as the default date interpreter.
    */
  def isoInterpreter: SwaggerCoreInterpreter = new SwaggerCoreInterpreter {
    override def dateFormatterExample: String = "'2011-12-03'"

    override def localDateFormatterExample: String = "2011-12-03T10:15:30"
  }

  def apply[A](gd: BonesSchema[A]): Schema[_] = isoInterpreter.apply(gd)
}

trait SwaggerCoreInterpreter {

  def dateFormatterExample: String
  def localDateFormatterExample: String

  private def copySchema(head: Schema[_], tail: Schema[_]): Schema[_] = {
    tail.getProperties.putAll(head.getProperties)
    tail.getRequired.addAll(head.getRequired)
    tail
  }

  def apply[A](gd: BonesSchema[A]): Schema[_] = gd match {
    case x: HListConvert[_, _, A] => fromValueDef(x).apply(new Schema())
  }

  def fromKvpHList[H <: HList, HL <: Nat](
      group: KvpHList[H, HL]): Schema[_] => Schema[_] = {
    group match {
      case KvpNil =>
        val objectSchema = new ObjectSchema()
          .nullable(false)
        schema =>
          objectSchema.name(schema.getName)
      case op: KvpHListHead[H, al, h, hl, t, tl] =>
        val head = fromKvpHList(op.head)
        val tail = fromKvpHList(op.tail)
        schema =>
          copySchema(head(schema), tail(schema))
      case op: KvpSingleValueHead[h, t, tl, o] =>
        val child = fromValueDef(op.fieldDefinition.op)
        val tail = fromKvpHList(op.tail)
        schema =>
          {
            val tailSchema = tail(schema)
            val childSchema = child(new Schema[h])
            tailSchema.addProperties(op.fieldDefinition.key, childSchema)
            if (!childSchema.getNullable) {
              tailSchema.addRequiredItem(op.fieldDefinition.key)
            }
            tailSchema
          }
      case op: KvpConcreteTypeHead[a, ht, nt, ho, xh, xl] =>
        val valueF = fromValueDef(op.hListConvert)
        schema =>
          {
            valueF(schema)
            schema
          }
    }
  }

  /**
    * Recursive method which builds up a Swagger Core Schema object from the DataClass definition.
    * @param vd The DataClass definition to convert to a Schema
    **/
  def fromValueDef[A](vd: KvpValue[A]): Schema[_] => Schema[_] = {
    vd match {
      case op: OptionalKvpValueDefinition[b] =>
        val oasSchema = fromValueDef(op.valueDefinitionOp)
        schema =>
          oasSchema(schema).nullable(true)

      case _: BooleanData =>
        val boolSchema = new BooleanSchema()
          .example(new java.lang.Boolean(true))
          .nullable(false)
        schema =>
          boolSchema.name(schema.getName)
      case _: StringData =>
        val stringSchema = new StringSchema()
          .example("ABC")
          .nullable(false)
        schema =>
          stringSchema.name(schema.getName)
      case ShortData(_) =>
        val shortSchema = new IntegerSchema()
          .nullable(false)
          .example(123)
          .format("int32")
          .maximum(java.math.BigDecimal.valueOf(Short.MaxValue.toLong))
          .minimum(java.math.BigDecimal.valueOf(Short.MinValue.toLong))
        schema => shortSchema.name(schema.getName)
      case IntData(_) =>
        val intSchema = new IntegerSchema()
          .nullable(false)
          .example(123)
          .format("int32")
        schema => intSchema.name(schema.getName)
      case _: LongData =>
        val intSchema = new IntegerSchema()
          .nullable(false)
          .example(123)
          .format("int64")
        schema =>
          intSchema.name(schema.getName)
      case _: UuidData =>
        val stringSchema = new UUIDSchema()
          .example(UUID.randomUUID().toString)
          .nullable(false)
        schema =>
          stringSchema.name(schema.getName)
      case _: LocalDateTimeData =>
        val stringSchema = new StringSchema()
          .example(dateFormatterExample)
          .nullable(false)
        schema =>
          stringSchema.name(schema.getName)
      case _: LocalDateData =>
        val stringSchema = new StringSchema()
          .example(dateFormatterExample)
          .nullable(false)
        schema =>
          stringSchema.name(schema.getName)
      case DoubleData(_) =>
        val stringSchema = new NumberSchema()
          .example("3.14")
          .nullable(false)
        schema =>
          stringSchema.name(schema.getName)
      case FloatData(_) =>
        val stringSchema = new NumberSchema()
          .example("3.14")
          .nullable(false)
        schema =>
          stringSchema.name(schema.getName)
      case _: BigDecimalData =>
        val stringSchema = new StringSchema()
          .example("3.14")
          .nullable(false)
        schema =>
          stringSchema.name(schema.getName)
      case _: ByteArrayData =>
        val baSchema = new ByteArraySchema()
          .example("0123456789abcdef")
          .nullable(false)
        schema =>
          baSchema.name(schema.getName)
      case ListData(definition, _) =>
        val items = fromValueDef(definition)
        val arraySchema = new ArraySchema()
        val itemSchema = items(new Schema())
        arraySchema.setItems(itemSchema)
        schema =>
          arraySchema.name(schema.getName)
      case ed: EitherData[a, b] =>
        val a = fromValueDef(ed.definitionA).apply(new Schema())
        val b = fromValueDef(ed.definitionB).apply(new Schema())
        val composedSchema = new ComposedSchema()
          .addAnyOfItem(a)
          .addAnyOfItem(b)
          .nullable(false)
        schema =>
          composedSchema.name(schema.getName)
      case esd: EnumerationData[e,a] =>
        val stringSchema = new StringSchema()
        esd.enumeration.values.foreach(v =>
          stringSchema.addEnumItemObject(v.toString))

        val nullableStringSchema = stringSchema.nullable(false)
          .example(esd.enumeration.values.head.toString)

        schema =>
          {
            nullableStringSchema.setName(schema.getName)
            nullableStringSchema
          }

      case x: SumTypeData[a, b] =>
        val obj = fromValueDef(x.from)
        schema =>
          obj(schema)

      case gd: KvpHListValue[h, hl] =>
        val obj = fromKvpHList(gd.kvpHList)
        schema =>
          obj(schema)
      case x: HListConvert[_, _, a] =>
        val fromF = fromKvpHList(x.from)
        schema =>
          {
            fromF(schema)
          }
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
              "(^([0-9A-Fa-f]{8}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{12})$)")
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
