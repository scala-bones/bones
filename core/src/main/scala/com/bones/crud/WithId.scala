package com.bones.crud

import com.bones.data.KeyValueDefinition
import com.bones.data.Value.{BonesSchema, KvpNil, HListConvert}
import shapeless.{HNil, Nat, Succ, ::}

object WithId {

  /**
    * Prefixes the schema with a particular ID type.  For instance, common ID types would be
    * integers, longs or UUIDs.
    * @param idDefinition The key/value definition of the id type.
    * @param entity The entity to which the id type is being added to
    * @tparam ID The type of the id (integer, long, UUID)
    * @tparam A The type of the entity (eg: a case class or HList)
    * @return a WithID type containing the id and entity.
    */
  def entityWithId[ID: Manifest, A](idDefinition: KeyValueDefinition[ID],
                                    entity: BonesSchema[A])
    : HListConvert[ID :: A :: HNil, Succ[Succ[Nat._0]], WithId[ID, A]] =
    entity match {
      case op: HListConvert[a, al, A] =>
        implicit val x: Manifest[A] = op.manifestOfA
        (idDefinition ::
          op :><:
          KvpNil).convert[WithId[ID, A]]
    }
}

/** Provides the ability to couple an ID with an Entity A, see companion class for BonesSchema */
case class WithId[ID, A](id: ID, a: A)
