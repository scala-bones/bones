package com.bones.crud

import com.bones.data.KeyValueDefinition
import com.bones.data.Value.{BonesSchema, KvpNil, XMapData}
import shapeless.{HNil, Nat, Succ, ::}

object WithId {
  def entityWithId[ID:Manifest,A](id: KeyValueDefinition[ID], entity: BonesSchema[A]): XMapData[ID :: A :: HNil, Succ[Succ[Nat._0]], WithId[ID,A]] =
    entity match {
      case op: XMapData[a,al,A] =>
        implicit val x = op.manifestOfA
        ( id ::
          op :><:
          KvpNil
          ).convert[WithId[ID,A]]
    }
}

/** Provides the ability to couple an ID with an Entity A, see companion class for BonesSchema */
case class WithId[ID,A](id: ID, a: A)
