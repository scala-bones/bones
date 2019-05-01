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
  case class StringInput(maxLength: Int) extends InputType[String]
  case class SelectInput(options: List[(Value,DisplayValue)]) extends InputType[String]
  case object Checkbox extends InputType[Boolean]
  case class LongInput(maxLength: Int) extends InputType[Long]
  case object BigDecimalInput extends InputType[BigDecimal]
  case object DateInput extends InputType[ZonedDateTime]
  case class TextArea(maxLength: Int) extends InputType[String]
  case class File() extends InputType[Array[Byte]]
  case class ReactComponentReference(childLabel: String) extends InputType[HList]



  case class ReactFormValue(label: String, required: Boolean, inputType: InputType[_]) {
    val inputName = keyToName(label)
  }
  case class ReactValueContainer(label: String, formValues: List[ReactFormValue], options: List[(String,List[String])]) {
    val name = keyToName(label)
  }

  def createComponents[A](bonesSchema: BonesSchema[A]): List[ReactValueContainer] = {
    bonesSchema match {
      case x: XMapData[a,al,b] => {
        val simpleName = x.manifestOfA.runtimeClass.getSimpleName
        val componentKey = Character.toLowerCase(simpleName.charAt(0)) + simpleName.substring(1)

        // We can ignore the first form value as we do not need it at the entry point
        val (_, components) = valueDefinition(x)(componentKey)
        components
      }
    }
  }

  def kvpHList[H<:HList,HL<:Nat](group: KvpHList[H,HL]): (List[ReactFormValue], List[ReactValueContainer]) = {
    group match {
      case KvpNil => (List.empty, List.empty)
      case op: KvpSingleValueHead[h, t, tl, a] =>
        val (headFormValue, headComponents) = valueDefinition(op.fieldDefinition.op)(op.fieldDefinition.key)
        val (tailFormValues, tailComponents) = kvpHList(op.tail)
        (headFormValue :: tailFormValues, headComponents ::: tailComponents)
      case op: KvpHListHead[a, al, h, hl, t, tl] =>
        val (headFormValues, headComponents) = kvpHList(op.head)
        val (tailFormValues, tailComponents) = kvpHList(op.tail)
        (headFormValues ::: tailFormValues, headComponents ::: tailComponents)
      case op: OptionalKvpHList[h,hl] => ???
      case op: KvpXMapDataHead[a,ht,nt,ho,xl,xll] =>
        val (headFormValues, headComponents) = kvpHList(op.xmapData.from)
        val (tailFormValues, tailComponents) = kvpHList(op.tail)
        (headFormValues ::: tailFormValues, headComponents ::: tailComponents)
    }
  }

  private def keyToName(key: String) : String = key

  def valueDefinition[A](fgo: ValueDefinitionOp[A]): Key => (ReactFormValue, List[ReactValueContainer]) =
    fgo match {
      case op: OptionalValueDefinition[a] =>
        val child = valueDefinition(op.valueDefinitionOp)
        key => {
          val (form, component) = child(key)
          (form.copy(required = false), component)
        }
      case ob: BooleanData =>
        key => ( ReactFormValue(key, false, Checkbox), List.empty )
      case rs: StringData =>
        key =>
          val maxLength = rs.validations.flatMap(v => v match {
            case MaxLength(l) => Some(l)
            case _ => None
          }).headOption

          val inputType = maxLength match {
            case Some(i) if i > 200 => TextArea(i)
            case Some(i) => StringInput(i)
            case None => TextArea(Int.MaxValue)
          }
          ( ReactFormValue( key, false, inputType), List.empty )
      case ri: LongData =>
        key =>
          ( ReactFormValue( key, false, LongInput(20)), List.empty )
      case uu: UuidData =>
        key => ( ReactFormValue( key, false, StringInput(36)), List.empty )
      case dd: DateTimeData =>
        key => ( ReactFormValue( key, false, DateInput), List.empty )
      case bd: BigDecimalData =>
        key => ( ReactFormValue( key, false, BigDecimalInput), List.empty )
      case ba: ByteArrayData =>
        key => ( ReactFormValue( key, false, File()), List.empty)
      case ld: ListData[t] => ???
      case ed: EitherData[a,b] => ???
      case esd: EnumerationStringData[a] =>
        val values: List[(Value, DisplayValue)] = esd.enumeration.values.map(v => (keyToName(v.toString), v.toString)).toList.sortBy(_._1)
        key => ( ReactFormValue( key, false, SelectInput(values)), List.empty )
      case esd: EnumStringData[a] =>
        val values: List[(Value, DisplayValue)] = esd.enums.map(v => (keyToName(v.toString), v.toString)).sortBy(_._1)
        key => ( ReactFormValue( key, false, SelectInput(values)), List.empty )
      case kvp: KvpHListData[h,hl] =>
        val (childForms, childComponents) = kvpHList(kvp.kvpHList)
        key => {
          val newComponent = ReactValueContainer(key, childForms, List.empty)
          (ReactFormValue( key, false, ReactComponentReference(key)), newComponent :: childComponents)
        }
      case t: XMapData[a, al, b] =>
        val (childForms, childComponents) = kvpHList(t.from)
        key => {
          val newComponent = ReactValueContainer( key, childForms, List.empty)
          (ReactFormValue( key, false, ReactComponentReference(key)), newComponent :: childComponents)
        }
      case s: SumTypeData[a,b] =>
        valueDefinition(s.from)

    }
}
