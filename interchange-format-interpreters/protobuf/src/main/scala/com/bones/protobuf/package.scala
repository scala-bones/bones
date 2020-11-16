package com.bones
import java.io.IOException

import com.bones.data.Error.ExtractionError
import com.google.protobuf.{CodedInputStream, CodedOutputStream}

package object protobuf {

  /******* Encoder Types ******/
  type FieldNumber = Int
  type LastFieldNumber = Int
  type ComputeSize = () => Int
  type Encode =
    CodedOutputStream => Either[List[IOException], CodedOutputStream]
  type ComputeEncode[A] = A => (ComputeSize, Encode)
  type EncodeToProto[A] = FieldNumber => (LastFieldNumber, ComputeEncode[A])

  def mapEncodeToProto[A, B](f: B => A, encodeToProto: EncodeToProto[A]): EncodeToProto[B] = {
    fieldNumber =>
      val (lastFieldNumber, computeEncode) = encodeToProto(fieldNumber)
      (lastFieldNumber, b => computeEncode(f(b)))
  }

  /******* Validator Types ******/
  /** Path to the value -- list of keys */
  type Tag = Int
  type CanReadTag = Boolean

  /**
    * Given the last field number from loading the previous value, as well as the path to the value
    * the interpreter will return the list of nested tags and the LastFieldNumber of the next values
    * to be read in.  Also return a function
    *
    * @tparam A
    */
  type ExtractFromProto[A] =
    (LastFieldNumber, Path[String]) => (
      List[Tag],
      LastFieldNumber,
      (CanReadTag, CodedInputStream) => (CanReadTag, Either[List[ExtractionError[String]], A])
    )
//  type ExtractHListFromProto[H <: HList] =
//    (LastFieldNumber, Path) => (
//      LastFieldNumber,
//      (CanReadTag, CodedInputStream) => (CanReadTag, Either[NonEmptyList[ExtractionError], H]))
//
//  type ExtractProductFromProto[C <: Coproduct] =
//    (LastFieldNumber, Path) => (
//      List[Tag],
//      LastFieldNumber,
//      (CanReadTag, CodedInputStream) => (CanReadTag, Either[NonEmptyList[ExtractionError], C]))

}
