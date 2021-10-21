package com.bones.jdbc.ideal

import com.bones.data.values._
import com.bones.jdbc.findUniqueConstraint
import com.bones.si.ideal._

object IdealScalaCoreInterpreter
    extends IdealValue[ScalaCoreValue]
    with BaseScalaCoreInterpreter[IdealDataType] {

  override def columns[A](
    alg: ScalaCoreValue[A]
  ): (TableCollection, List[UniqueGroup], ColumnName, Option[Description]) => (
    TableCollection,
    List[UniqueGroup]
  ) = {
    val uniqueConstraint = findUniqueConstraint(alg.validations)
    defaultColumns(matchScalaCoreValue(alg), uniqueConstraint)
  }

  override def boolDataToOut(booleanData: BooleanData): IdealDataType = BooleanType

  override def intDataToOut(intData: IntData): IdealDataType = IntegerType()

  override def longDataToOut(longData: LongData): IdealDataType = LongType()

  override def shortDataToOut(shortData: ShortData): IdealDataType = SmallIntType

  override def stringDataToOut(stringData: StringData): IdealDataType =
    StringType.unbounded //TODO: Determine when this should be bounded

  override def floatDataToOut(floatData: FloatData): IdealDataType = RealType

  override def doubleDataToOut(doubleData: DoubleData): IdealDataType = DoubleType

  override def bigDecimalToOut(bigDecimalData: BigDecimalData): IdealDataType =
    NumericType(10, 2) //TODO: can we determine scale?

  override def byteArrayToOut(byteArrayData: ByteArrayData): IdealDataType = BinaryType(None)

  override def enumerationToOut[A](enumerationData: EnumerationData[_, A]): IdealDataType =
    StringType.unbounded //TODO: Determine when this should be bounded
}
