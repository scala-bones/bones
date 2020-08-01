package com.bones.jdbc.ideal

import com.bones.data.values.{Base64Data, BaseCustomStringValue, CreditCardData, CustomStringValue, EmailData, GuidData, HexStringData, HostnameData, IpV4Data, IpV6Data, UriData, UrlData}
import com.bones.si.ideal.{IdealColumn, IdealDataType, StringType}

object IdealCustomStringInterpreter extends IdealValue[CustomStringValue] with BaseCustomStringValue[IdealDataType] {


  override def columns[A](alg: CustomStringValue[A]): (TableCollection, ColumnName, Option[Description]) => TableCollection =
    (tableCollection, name, description) =>
    {
      val newType = matchCustomStringValue(alg)
      val newColumn = IdealColumn(name, newType, false, description)
      tableCollection.prependColumn(newColumn)
    }


  override def emailData(emailData: EmailData): IdealDataType = StringType.unbounded

  override def guidData(guidData: GuidData): IdealDataType = StringType(37)

  override def creditCardData(creditCardData: CreditCardData): IdealDataType = StringType(16)

  override def hexStringData(hexStringData: HexStringData): IdealDataType = StringType.unbounded

  override def base64Data(base64Data: Base64Data): IdealDataType = StringType.unbounded

  override def hostnameData(hostnameData: HostnameData): IdealDataType = StringType.unbounded

  override def uriData(uriData: UriData): IdealDataType = StringType.unbounded

  override def urlData(urlData: UrlData): IdealDataType = StringType.unbounded

  override def ip4vData(ipV4Data: IpV4Data): IdealDataType = StringType(15)

  override def ip46Data(ipV6Data: IpV6Data): IdealDataType = StringType(45)
}
