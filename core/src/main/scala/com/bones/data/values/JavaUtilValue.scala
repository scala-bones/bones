package com.bones.data.values

import java.util.UUID

import com.bones.KvpValue
import com.bones.validation.ValidationDefinition.ValidationOp
import shapeless.Coproduct
import shapeless.ops.coproduct.Inject

abstract class JavaUtilValue[A:Manifest] extends KvpValue[A] {
  override val manifestOfA: Manifest[A] = manifest[A]
}

final case class UuidData(validations: List[ValidationOp[UUID]])
  extends JavaUtilValue[UUID]

trait BaseJavaUtilInterpreter[OUT] {
  def matchJavaUtilValue[A](alg: JavaUtilValue[A]): OUT = {
    alg match {
      case ud: UuidData => uuidData(ud)
    }
  }

  def uuidData(uuidData: UuidData): OUT
}


trait JavaUtilSugar {
  /** Indicates that the data tied to this key is a UUID type that must pass the specified validations. */
  def uuid(v: ValidationOp[UUID]*): UuidData = UuidData(v.toList)

  /** Alias for UUID without validations */
  val uuid: UuidData = UuidData(List.empty)

}

trait JavaUtilInjectedSugar[ALG[_] <: Coproduct] {
  def javaUtilInjected[A]: Inject[ALG[A], JavaUtilValue[A]]

  /** Indicates that the data tied to this key is a UUID type that must pass the specified validations. */
  def uuid(v: ValidationOp[UUID]*): ALG[UUID] =
    javaUtilInjected(UuidData(v.toList))

  /** Alias for UUID without validations */
  val uuid: ALG[UUID] = uuid()
}
