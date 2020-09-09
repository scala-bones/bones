package com.bones.swagger

import java.util.{Base64, UUID}

import com.bones.data.template.KvpCollectionMatch
import com.bones.data.values.CNilF
import com.bones.data.{KvpCoNil, KvpCoproduct, KvpCoproductCollectionHead, _}
import com.bones.swagger.SwaggerCoreInterpreter.SwaggerSchemas
import com.bones.validation.ValidationDefinition.{
  InvalidValue,
  ValidValue,
  ValidationOp,
  BigDecimalValidation => bdv,
  LongValidation => iv,
  StringValidation => sv
}
import io.swagger.v3.oas.models.media._
import shapeless.{:+:, Coproduct, HList, Inl, Inr, Nat, ::}

import scala.jdk.CollectionConverters._

object SwaggerCoreInterpreter {

  type Name = String

  val exampleUuid = UUID.fromString("322dd565-0a28-4959-9b7e-42ba84149870")

  object SwaggerSchemas {
    def apply[S <: Schema[_]](mainSchema: S): SwaggerSchemas[S] =
      SwaggerSchemas(mainSchema, List.empty)
  }

  /** Responsible for keeping track of mainSchemas and referenceSchemas */
  case class SwaggerSchemas[+S <: Schema[_]](
    mainSchema: S,
    referenceSchemas: List[(String, Schema[_])])

  object CustomSwaggerInterpreter {

    /** using kind projector allows us to create a new interpreter by merging two existing interpreters.
      * see https://stackoverflow.com/a/60561575/387094
      * */
    def merge[L[_], R[_] <: Coproduct, A, OUT](
      li: CustomSwaggerInterpreter[L],
      ri: CustomSwaggerInterpreter[R]): CustomSwaggerInterpreter[Lambda[A => L[A] :+: R[A]]] =
      new CustomSwaggerInterpreter[Lambda[A => L[A] :+: R[A]]] {

        override def toSchema[A](
          lr: L[A] :+: R[A],
          description: Option[String],
          example: Option[A]): Name => SwaggerSchemas[Schema[_]] =
          lr match {
            case Inl(l) => li.toSchema(l, description, example)
            case Inr(r) => ri.toSchema(r, description, example)
          }
      }

    implicit class InterpreterOps[ALG[_]](val base: CustomSwaggerInterpreter[ALG]) extends AnyVal {
      def ++[R[_] <: Coproduct](
        r: CustomSwaggerInterpreter[R]): CustomSwaggerInterpreter[Lambda[A => ALG[A] :+: R[A]]] =
        merge(base, r)
    }

    object CNilCustomSwaggerInterpreter extends CustomSwaggerInterpreter[CNilF] {
      override def toSchema[A](
        vd: CNilF[A],
        description: Option[String],
        example: Option[A]): Name => SwaggerSchemas[Schema[_]] =
        sys.error("Unreachable code")
    }

  }

  trait CustomSwaggerInterpreter[ALG[_]] {
    def toSchema[A](
      vd: ALG[A],
      description: Option[String],
      example: Option[A]): Name => SwaggerSchemas[Schema[_]]
  }

  def addBooleanSchema(
    name: String,
    description: Option[String],
    example: Option[Boolean],
    applyValidationDescription: Schema[_] => Schema[_]) = {
    val main = new BooleanSchema()
      .example(example.getOrElse(new java.lang.Boolean(true)))
      .description(description.getOrElse("value of type boolean"))
      .nullable(false)
      .name(name)
    val mainWithValidation = applyValidationDescription(main)
    SwaggerSchemas(mainWithValidation)
  }

  def addDateSchema(
    name: String,
    description: String,
    example: String,
    applyValidationDescription: Schema[_] => Schema[_]) = {
    val date = new DateSchema()
      .example(example)
      .nullable(false)
      .name(name)
      .description(description)
    val mainWithValidation = applyValidationDescription(date)
    SwaggerSchemas(mainWithValidation)
  }

  def addDateTimeSchema(
    name: String,
    description: String,
    example: String,
    applyValidationDescription: Schema[_] => Schema[_]) = {
    val date = new DateTimeSchema()
      .example(example)
      .nullable(false)
      .name(name)
      .description(description)
    val mainWithValidation = applyValidationDescription(date)
    SwaggerSchemas(mainWithValidation)
  }

  def addStringSchema(
    name: String,
    description: String,
    example: String,
    applyValidationDescription: Schema[_] => Schema[_]) = {
    val main = new StringSchema()
      .example(example)
      .nullable(false)
      .name(name)
      .description(description)
    val mainWithValidation = applyValidationDescription(main)
    SwaggerSchemas(mainWithValidation)
  }

  def addShortSchema(
    name: String,
    description: String,
    example: Short,
    applyValidationDescription: Schema[_] => Schema[_]) = {
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

  def addIntSchema(
    name: String,
    description: String,
    example: Int,
    applyValidationDescription: Schema[_] => Schema[_]) = {
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

  def addLongSchema(
    name: String,
    description: String,
    example: Long,
    applyValidationDescription: Schema[_] => Schema[_]) = {
    val intSchema = new IntegerSchema()
      .nullable(false)
      .example(Long.box(example))
      .description(description)
      .format("int64")
      .name(name)
    val withValidation = applyValidationDescription(intSchema)
    SwaggerSchemas(withValidation)
  }

  def addUuidSchema(
    name: String,
    description: String,
    example: UUID,
    applyValidationDescription: Schema[_] => Schema[_]) = {
    val uuidSchema = new UUIDSchema()
      .example(example)
      .description(description)
      .nullable(false)
      .name(name)
    val withValidation = applyValidationDescription(uuidSchema)
    SwaggerSchemas(withValidation)
  }

  def addNumberSchema(
    name: String,
    description: String,
    example: String,
    applyValidationDescription: Schema[_] => Schema[_]) = {
    val numberSchema = new NumberSchema()
      .example(example)
      .description(description)
      .nullable(false)
      .name(name)
    val withValidation = applyValidationDescription(numberSchema)
    SwaggerSchemas(withValidation)
  }

  def addBase64ByteArraySchema(
    name: String,
    description: String,
    example: Array[Byte],
    applyValidationDescription: Schema[_] => Schema[_]) = {
    val baSchema = new ByteArraySchema()
      .example(Base64.getEncoder.encodeToString(example))
      .description(description)
      .nullable(false)
      .name(name)
    val withValidation = applyValidationDescription(baSchema)
    SwaggerSchemas(withValidation)
  }

  def addEnumerationData(
    name: String,
    description: String,
    example: String,
    enumerations: List[String],
    applyValidationDescription: Schema[_] => Schema[_]) = {
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
      case sv.Uppercase       => identity
      case sv.Token           => identity
      case sv.Lowercase       => identity

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

/**
  * Responsible for creating the data portion of the Swagger schema.
  */
trait SwaggerCoreInterpreter[ALG[_]] extends KvpCollectionMatch[ALG, SwaggerSchemas[ObjectSchema]] {

  import SwaggerCoreInterpreter._

  def customInterpreter: CustomSwaggerInterpreter[ALG]

  private def copySchema(head: Schema[_], tail: ObjectSchema): ObjectSchema = {
    Option(head.getProperties).foreach(_.asScala.toList.foreach(prop =>
      tail.addProperties(prop._1, prop._2)))
    Option(head.getRequired).foreach(_.asScala.toList.foreach(req => tail.addRequiredItem(req)))
    tail
  }

  def generateSchemas[A](
    collection: KvpCollection[ALG, A]
  ): Name => List[(Name, Schema[_])] = name => {
    val schemas = fromKvpCollection(collection)
    schemas.mainSchema.name(name)
    (name, schemas.mainSchema) :: schemas.referenceSchemas
  }

  private def fromKvpCoproduct[C <: Coproduct](
    co: KvpCoproduct[ALG, C]
  ): SwaggerSchemas[ComposedSchema] =
    co match {
      case _: KvpCoNil[_] => SwaggerSchemas(new ComposedSchema())
      case co: KvpCoproductCollectionHead[ALG, a, c, o] @unchecked => {
        val name = co.manifestOfHead.runtimeClass.getSimpleName
        val lSchema = fromKvpCollection(co.kvpCollection)
        val composedSchema = fromKvpCoproduct(co.kvpTail)
        val objectSchema = new ObjectSchema().$ref(name)
        val mainSchema = composedSchema.mainSchema.addOneOfItem(objectSchema)
        SwaggerSchemas(
          mainSchema,
          (name, lSchema.mainSchema) :: lSchema.referenceSchemas ::: composedSchema.referenceSchemas)
      }
    }

  override def kvpCoproduct[C <: Coproduct](
    kvp: KvpCoproduct[ALG, C]): SwaggerSchemas[ObjectSchema] = {
    val composedSchema = fromKvpCoproduct(kvp)
    val name =
      KvpCollection.headManifest(kvp).map(_.runtimeClass.getSimpleName).getOrElse("unknown")
    val main = new ObjectSchema()
    main.$ref(name)
//    validations(kvp.)(main)
    SwaggerSchemas(main, (name, composedSchema.mainSchema) :: composedSchema.referenceSchemas)
  }

  override def kvpNil(kvp: KvpNil[ALG]): SwaggerSchemas[ObjectSchema] = {
    val schema = new ObjectSchema()
    schema.nullable(false)
    SwaggerSchemas(schema)
  }

  override def kvpSingleValueHead[H, T <: HList, TL <: Nat, O <: H :: T](
    kvp: KvpSingleValueHead[ALG, H, T, TL, O]): SwaggerSchemas[ObjectSchema] = {

    val tailSchemas = fromKvpCollection(kvp.tail)

    val headSchemas = kvp.head match {
      case Left(keyDef) => {
        determineValueDefinition(
          keyDef.dataDefinition,
          keyDef.description,
          keyDef.example
        )(keyDef.key)
      }
      case Right(kvpCollection) => {
        fromKvpCollection(kvpCollection)
      }
    }
    val schema = copySchema(headSchemas.mainSchema, tailSchemas.mainSchema)
    SwaggerSchemas(schema, headSchemas.referenceSchemas ::: tailSchemas.referenceSchemas)
  }

  override def kvpWrappedHList[A, H <: HList, HL <: Nat](
    wrappedHList: KvpWrappedHList[ALG, A, H, HL]): SwaggerSchemas[ObjectSchema] = {
    val schema = fromKvpCollection(wrappedHList.wrappedEncoding)
    schema.mainSchema.name(wrappedHList.manifestOfA.runtimeClass.getSimpleName)
    schema
  }

  override def kvpWrappedCoproduct[A, C <: Coproduct](
    wrappedCoproduct: KvpWrappedCoproduct[ALG, A, C]): SwaggerSchemas[ObjectSchema] = {
    val schema = fromKvpCollection(wrappedCoproduct.wrappedEncoding)
    schema.mainSchema.name(wrappedCoproduct.manifestOfA.runtimeClass.getSimpleName)
    schema
  }

  override def kvpHListCollectionHead[
    HO <: HList,
    NO <: Nat,
    H <: HList,
    HL <: Nat,
    T <: HList,
    TL <: Nat](
    kvp: KvpHListCollectionHead[ALG, HO, NO, H, HL, T, TL]): SwaggerSchemas[ObjectSchema] = {
    val headSchemas = fromKvpCollection(kvp.head)
    val tailSchemas = fromKvpCollection(kvp.tail)
    val schema = copySchema(headSchemas.mainSchema, tailSchemas.mainSchema)
    SwaggerSchemas(schema, headSchemas.referenceSchemas ::: tailSchemas.referenceSchemas)
  }

  def determineValueDefinition[A](
    value: Either[PrimitiveWrapperValue[ALG, A], ALG[A]],
    description: Option[String],
    example: Option[A]
  ): Name => SwaggerSchemas[Schema[_]] =
    value match {
      case Left(kvp)  => valueDefinition(kvp, description, example)
      case Right(alg) => customInterpreter.toSchema(alg, description, example)
    }

  /**
    * Recursive method which builds up a Swagger Core Schema object from the DataClass definition.
    *
    * @param vd The DataClass definition to convert to a Schema
    **/
  def valueDefinition[A](
    vd: PrimitiveWrapperValue[ALG, A],
    description: Option[String],
    example: Option[A]): Name => SwaggerSchemas[Schema[_]] = {
    vd match {
      case op: OptionalValue[ALG, b] @unchecked =>
        name =>
          val oasSchema =
            determineValueDefinition[b](op.valueDefinitionOp, description, None)(name)
          oasSchema.copy(mainSchema = oasSchema.mainSchema.nullable(true))

      case ld: ListData[ALG, t] @unchecked =>
        name =>
          val itemSchema =
            determineValueDefinition(ld.tDefinition, description, None)(name)
          val arraySchema = new ArraySchema()
          arraySchema.setItems(itemSchema.mainSchema)
          arraySchema.description(description.getOrElse("value of type list"))
          arraySchema.name(name)
          val withValidation = validations(ld.validations)(arraySchema)
          SwaggerSchemas(withValidation, itemSchema.referenceSchemas)
      case ed: EitherData[ALG, a, b] @unchecked =>
        name =>
          val a =
            determineValueDefinition[a](ed.definitionA, description, None)("left" + name.capitalize)
          val b =
            determineValueDefinition[b](ed.definitionB, description, None)(
              "right" + name.capitalize)
          val composedSchema = new ComposedSchema()
            .addAnyOfItem(a.mainSchema)
            .addAnyOfItem(b.mainSchema)
            .nullable(false)
            .description(description.getOrElse("value of type either"))
            .name(name)
          SwaggerSchemas(composedSchema, a.referenceSchemas ::: b.referenceSchemas)
      case gd: KvpCollectionValue[ALG, a] @unchecked =>
        name =>
          val schemas = fromKvpCollection(gd.kvpCollection)
          schemas.mainSchema.name(name).description(description.getOrElse("value of type object"))
          validations(gd.validations)(schemas.mainSchema)
          schemas

    }
  }

}
