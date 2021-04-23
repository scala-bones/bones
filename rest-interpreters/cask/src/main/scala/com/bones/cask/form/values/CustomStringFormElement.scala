package com.bones.cask.form.values

import com.bones.Util
import com.bones.cask.form.{FormElement, FormElementEncoder}
import com.bones.data.values.{
  Base64Data,
  CreditCardData,
  CustomStringValue,
  EmailData,
  GuidData,
  HexStringData,
  HostnameData,
  IpV4Data,
  IpV6Data,
  UriData,
  UrlData
}
import scalatags.Text
import scalatags.Text.all._

object CustomStringFormElement extends FormElement[CustomStringValue] {
  override def generateFormElement[A](
    kvp: CustomStringValue[A],
    path: List[String]): FormElementEncoder[A] = {

    val func: A => Text.TypedTag[String] = kvp match {
      case ed: EmailData =>
        (s: String) =>
          input(`type` := "email", `class` := "bones_email", value := s)
      case gd: GuidData =>
        (s: String) =>
          input(`type` := "text", `class` := "bones_guid", size := "16", value := s)
      case cd: CreditCardData =>
        (s: String) =>
          input(`type` := "text", `class` := "bones_credit-card", size := "16", value := s)
      case hd: HexStringData =>
        (s: String) =>
          textarea(value := s, `class` := "bones_hex")
      case bd: Base64Data =>
        (s: String) =>
          textarea(value := s, `class` := "bones_base64")
      case hd: HostnameData =>
        (s: String) =>
          input(`type` := "text", `class` := "bones_hostname", size := "200", value := s)
      case ud: UriData =>
        (s: String) =>
          input(`type` := "text", `class` := "bones_uri", size := "200", value := s)
      case ud: UrlData =>
        (s: String) =>
          input(`type` := "text", `class` := "bones_url", size := "200", value := s)
      case id: IpV4Data =>
        (s: String) =>
          input(`type` := "text", `class` := "bones_ipv4", size := "16", value := s)
      case id: IpV6Data =>
        (s: String) =>
          input(`type` := "text", `class` := "bones_ipv6", size := "39", value := s)
    }

    (a: A) =>
      withLabel(path, func(a))

  }
}
