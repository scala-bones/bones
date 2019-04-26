package com.bones.react

import java.time.ZonedDateTime

import com.bones.data.Value._
import com.bones.validation.ValidationDefinition.StringValidation.MaxLength
import shapeless.{HList, Nat}

object ValidationInterpreter {

}
object FormInterpreter {

  object InputType2 extends Enumeration {
    type InputType = Value
    val StringInput, NumberInput = Value
  }

  type Value = String
  type DisplayValue = String
  type Key = String

  trait InputType[A]
  case class StringInput(maxLength: Int, defaultValue: Option[String]) extends InputType[String]
  case class SelectInput(options: List[(Value,DisplayValue)], default: Option[(Value,DisplayValue)]) extends InputType[String]
  case class Checkbox(default: Option[Boolean]) extends InputType[Boolean]
  case class LongInput(default: Option[Long]) extends InputType[Long]
  case class BigDecimalInput(default: Option[BigDecimal]) extends InputType[BigDecimal]
  case class DateInput(default: Option[ZonedDateTime]) extends InputType[ZonedDateTime]
  case class TextArea(maxLength: Int, default: Option[String]) extends InputType[String]
  case class File() extends InputType[Array[Byte]]
  case class ReactComponentReference(parentLabel: String) extends InputType[HList]



  case class ReactFormValue(inputName: String, label: String, required: Boolean, inputType: InputType[_])
  case class ReactComponent(name: String, label: String, formValues: List[ReactFormValue])

  def createComponents[A](bonesSchema: BonesSchema[A]): List[ReactComponent] = {
    bonesSchema match {
      case x: XMapData[a,al,b] => {
        val componentKey = x.manifestOfA.runtimeClass.getSimpleName
        // We can ignore the first form value as we do not need it at the entry point
        val (_, components) = valueDefinition(x)(componentKey)
        components
      }
    }
  }

  def kvpGroup[H<:HList,HL<:Nat](group: KvpGroup[H,HL]): (List[ReactFormValue], List[ReactComponent]) = {
    group match {
      case KvpNil => (List.empty, List.empty)
      case op: KvpSingleValueHead[h, t, tl, a] =>
        val (headFormValue, headComponents) = valueDefinition(op.fieldDefinition.op)(op.fieldDefinition.key)
        val (tailFormValues, tailComponents) = kvpGroup(op.tail)
        (headFormValue :: tailFormValues, headComponents ::: tailComponents)
      case op: KvpGroupHead[a, al, h, hl, t, tl] =>
        val (headFormValues, headComponents) = kvpGroup(op.head)
        val (tailFormValues, tailComponents) = kvpGroup(op.tail)
        (headFormValues ::: tailFormValues, headComponents ::: tailComponents)
      case op: OptionalKvpGroup[h,hl] => ???
      case op: KvpXMapDataHead[a,ht,nt,ho,xl,xll] =>
        val (headFormValues, headComponents) = kvpGroup(op.xmapData.from)
        val (tailFormValues, tailComponents) = kvpGroup(op.tail)
        (headFormValues ::: tailFormValues, headComponents ::: tailComponents)
    }
  }

  private def keyToName(key: String) : String = key

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): Key => (ReactFormValue, List[ReactComponent]) =
    fgo match {
      case op: OptionalValueDefinition[a] =>
        val child = valueDefinition(op.valueDefinitionOp)
        key => {
          val (form, component) = child(key)
          (form.copy(required = false), component)
        }
      case ob: BooleanData =>
        key => ( ReactFormValue(key, keyToName(key), false, Checkbox(None)), List.empty )
      case rs: StringData =>
        key =>
          val maxLength = rs.validations.flatMap(v => v match {
            case MaxLength(l) => Some(l)
          }).headOption

          val inputType = maxLength match {
            case Some(i) if i > 50 => TextArea(i, None)
            case Some(i) => StringInput(i, None)
            case None => TextArea(Int.MaxValue, None)
          }
          ( ReactFormValue(key, keyToName(key), false, inputType), List.empty )
      case ri: LongData =>
        key =>
          ( ReactFormValue(key, keyToName(key), false, LongInput(None)), List.empty )
      case uu: UuidData =>
        key => ( ReactFormValue(key, keyToName(key), false, StringInput(36, None)), List.empty )
      case dd: DateTimeData =>
        key => ( ReactFormValue(key, keyToName(key), false, DateInput(None)), List.empty )
      case bd: BigDecimalData =>
        key => ( ReactFormValue(key, keyToName(key), false, BigDecimalInput(None)), List.empty )
      case ba: ByteArrayData =>
        key => ( ReactFormValue(key, keyToName(key), false, File()), List.empty)
      case ld: ListData[t] => ???
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] =>
        val values: List[(Value, DisplayValue)] = esd.enumeration.values.map(v => (v.toString, keyToName(v.toString))).toList.sortBy(_._1)
        key => ( ReactFormValue(key, keyToName(key), false, SelectInput(values, None)), List.empty )
      case esd: EnumStringData[a] =>
        val values: List[(Value, DisplayValue)] = esd.enums.map(v => (v.toString, keyToName(v.toString))).sortBy(_._1)
        key => ( ReactFormValue(key, keyToName(key), false, SelectInput(values, None)), List.empty )
      case kvp: KvpGroupData[h,hl] =>
        val (childForms, childComponents) = kvpGroup(kvp.kvpGroup)
        key => {
          val newComponent = ReactComponent(keyToName(key), key, childForms)
          (ReactFormValue(keyToName(key), key, false, ReactComponentReference(key)), newComponent :: childComponents)
        }
      case t: XMapData[a, al, b] =>
        val (childForms, childComponents) = kvpGroup(t.from)
        key => {
          val newComponent = ReactComponent(keyToName(key), key, childForms)
          (ReactFormValue(keyToName(key), key, false, ReactComponentReference(key)), newComponent :: childComponents)
        }
      case s: SumTypeData[a,b] =>
        valueDefinition(s.from)

    }
}
