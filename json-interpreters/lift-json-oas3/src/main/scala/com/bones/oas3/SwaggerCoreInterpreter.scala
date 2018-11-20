package com.bones.oas3

import java.time.LocalDateTime
import java.util.UUID

import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.{InvalidValue, OptionalValidation, ValidValue, ValidationOp}
import io.swagger.v3.oas.models.media._
import com.bones.validation.ValidationDefinition.{BigDecimalValidation => bdv, IntValidation => iv, StringValidation => sv}
import shapeless.{HList, Nat}


object SwaggerCoreInterpreter {

  private def copySchema(head: Schema[_], tail: Schema[_]): Schema[_] = {
    tail.getProperties.putAll(head.getProperties)
    tail.getRequired.addAll(head.getRequired)
    tail
  }

  /** The entry point, converts a Value definition into a Schema-Core Schema */
  def apply[A](gd: Value[A]): Schema[_] = {
    gd match {
      case x: XMapData[_,_,a] => fromKvpGroup(x).apply(new Schema())
    }
  }

  protected def fromKvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]) : Schema[_] => Schema[_] = {
    group match {
      case KvpNil =>
        val objectSchema = new ObjectSchema()
          .nullable(false)
        schema => objectSchema.name(schema.getName)
      case op: KvpGroupHead[H, al, h, hl, t, tl] =>
        val head = fromKvpGroup(op.head)
        val tail = fromKvpGroup(op.tail)
        schema => copySchema(head(schema), tail(schema))
      case op: KvpSingleValueHead[h, t, tl, o, ol] =>
        val child = fromValueDef(op.fieldDefinition.op)
        val tail = fromKvpGroup(op.tail)

        schema => {
          val tailSchema = tail(schema)
          val childSchema = child(new Schema[h])
          tailSchema.addProperties(op.fieldDefinition.key, childSchema)
          if (! childSchema.getNullable) {
            tailSchema.addRequiredItem(op.fieldDefinition.key)
          }
          tailSchema
        }
      case t: XMapData[a,al,b] =>
        val obj = fromKvpGroup(t.from)
        schema => obj(schema)


    }
  }

  /**
    * Recursive method which builds up a Swagger Core Schema object from the Value definition.
    * @param vd The Value definition to convert to a Schema
    **/
  protected def fromValueDef[A](vd: ValueDefinitionOp[A]): Schema[_] => Schema[_] = {
    vd match {
      case op: OptionalValueDefinition[b] =>
        val oasSchema = fromValueDef(op.valueDefinitionOp)
        schema => oasSchema(schema).nullable(true)

      case _: BooleanData =>
        val boolSchema = new BooleanSchema()
          .example(new java.lang.Boolean(true))
          .nullable(false)
        schema => boolSchema.name(schema.getName)
      case _: StringData =>
        val stringSchema = new StringSchema()
          .example("ABC")
          .nullable(false)
        schema => stringSchema.name(schema.getName)
      case _: IntData =>
        val intSchema = new IntegerSchema()
          .nullable(false).example(123).format("int64")
        schema => intSchema.name(schema.getName)
      case _: UuidData =>
        val stringSchema = new UUIDSchema()
          .example(UUID.randomUUID().toString).nullable(false)
        schema => stringSchema.name(schema.getName)
      case dd: DateData =>
        val stringSchema = new StringSchema()
          .example(dd.dateFormat.format(LocalDateTime.now())).nullable(false)
        schema => stringSchema.name(schema.getName)
      case _: BigDecimalFromString =>
        val stringSchema = new StringSchema()
          .example("3.14").nullable(false)
        schema => stringSchema.name(schema.getName)
      case _: DoubleData =>
        val numberSchema = new NumberSchema()
          .example(3.14).nullable(false)
        schema => numberSchema.name(schema.getName)
      case ListData(definition, validations) =>
        val items = fromValueDef(definition)
        val arraySchema = new ArraySchema()
        val itemSchema = items(new Schema())
        arraySchema.setItems(itemSchema)
        schema => arraySchema.name(schema.getName)
      case ed: EitherData[a,b] =>
        val a = fromValueDef(ed.definitionA).apply(new Schema())
        val b = fromValueDef(ed.definitionB).apply(new Schema())
        val composedSchema = new ComposedSchema()
          .addAnyOfItem(a)
          .addAnyOfItem(b)
          .nullable(false)
        schema => composedSchema.name(schema.getName)
      case esd: EnumerationStringData[a] =>
        val stringSchema = new StringSchema()
          .nullable(false).example(esd.enumeration.values.head.toString).asInstanceOf[StringSchema]
        schema => {
          stringSchema.setName(schema.getName)
          esd.enumeration.values.foreach(v => stringSchema.addEnumItemObject(v.toString))
          stringSchema
        }

      case esd: EnumStringData[a] =>
        val stringSchema = new StringSchema()
          .nullable(false).example(esd.enums.head.toString).asInstanceOf[StringSchema]
        schema => {
          stringSchema.setName(schema.getName)
          esd.enums.foreach(v => stringSchema.addEnumItemObject(v.toString))
          stringSchema
        }

      case x: SumTypeData[a,b] =>
        val obj = fromValueDef(x.from)
        schema => obj(schema)

      case gd: KvpGroupData[h,hl] =>
        val obj = fromKvpGroup(gd.kvpGroup)
        schema => obj(schema)

    }
  }

  /**
    * Responsible for adding validation specific details to the SwaggerCore files.
    * @param op The validation from we will add to the
    * @tparam A
    * @return
    */
  def validation[A](op: ValidationOp[A]): Schema[_] => Schema[_] = {
    op match {
      case OptionalValidation(_) => schema => schema.setNullable(true); schema
      case ValidValue(values) =>
        schema => {
          val typedSchema = schema.asInstanceOf[Schema[A]]
          values.foreach(typedSchema.addEnumItemObject)
          typedSchema
        }
      case InvalidValue(values) =>
        val invalidDescription = s"These values are not allowed: ${values.mkString("('", ",", "')")}."
        schema => {
          val description = schema.getDescription
          val newDescription = if (description.isEmpty) invalidDescription else description + " " + invalidDescription
          schema.setDescription(newDescription)
          schema
        }
      case sv.IsAlphanum() =>
        schema => schema.pattern("^[:alnum:]+$"); schema
      case sv.MinLength(min) => schema => schema.minLength(min)
      case sv.MaxLength(max) => schema => schema.maxLength(max)
      case sv.MatchesRegex(r) => schema => schema.pattern(r.toString())
      case sv.Length(l) => schema => schema.minLength(l).maxLength(l)
      case sv.Custom(_,_,_) => identity
      case sv.Guid() => schema =>
        schema.minLength(36).maxLength(36)
          .pattern("(^([0-9A-Fa-f]{8}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{4}[-][0-9A-Fa-f]{12})$)")
          .format("guid")
      case sv.Uppercase() => identity
      case sv.CreditCard() => identity
      case sv.Token() => identity
      case sv.Email() =>
        val emailSchema = new EmailSchema()
        schema => emailSchema.setName(schema.getName); emailSchema
      case sv.Hex() => identity
      case sv.Base64() => identity
      case sv.Hostname() => _.format("hostname")
      case sv.Ipv4() => _.format("ipv4")
      case sv.Lowercase() => identity
      case sv.Uri() => _.format("uri")

      case iv.Between(min, max) => _.exclusiveMinimum(true).exclusiveMaximum(true)
        .minimum(new java.math.BigDecimal(min))
        .maximum(new java.math.BigDecimal(max))
      case iv.Greater(gt) => _.minimum(new java.math.BigDecimal(gt)).exclusiveMinimum(true)
      case iv.Less(lt) => _.maximum(new java.math.BigDecimal(lt)).exclusiveMaximum(true)
      case iv.Max(max) => _.maximum(new java.math.BigDecimal(max))
      case iv.Min(min) => _.minimum(new java.math.BigDecimal(min))
      case iv.Multiple(x) => _.multipleOf(new java.math.BigDecimal(x))
      case iv.Negative() => _.exclusiveMaximum(true).maximum(java.math.BigDecimal.ZERO)
      case iv.Positive() => _.exclusiveMinimum(true).minimum(java.math.BigDecimal.ZERO)

      case bdv.Max(max) => _.maximum(max.bigDecimal)
      case bdv.Min(min) => _.minimum(min.bigDecimal)

      case _ => identity

    }
  }

}


