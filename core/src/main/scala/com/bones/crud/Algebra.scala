package com.bones.crud

import com.bones.data.Value.BonesSchema

/**
  * Defines the algebra for the CRUD actions.
  */
object Algebra {

  object ServiceOps {
    def withPath(path: String) = ServiceOps(path, None, None, None, None)
    def basicCrud[A, E](path: String,
                        schema: BonesSchema[A],
                        errorSchema: BonesSchema[E]) = {
      ServiceOps
        .withPath(path)
        .withCreate(schema, schema, errorSchema)
        .withRead(schema, errorSchema)
        .withUpdate(schema, schema, errorSchema)
        .withDelete(schema, errorSchema)
    }
  }

  case class ServiceOps[CI, CO, CE, RO, RE, UI, UO, UE, DO, DE](
      path: String,
      createOperation: Option[Create[CI, CO, CE]],
      readOperation: Option[Read[RO, RE]],
      updateOperation: Option[Update[UI, UO, UE]],
      deleteOperation: Option[Delete[DO, DE]]
  ) {
    def withCreate[I, O, E](
        inputSchema: BonesSchema[I],
        successSchema: BonesSchema[O],
        errorSchema: BonesSchema[E]
    ): ServiceOps[I, O, E, RO, RE, UI, UO, UE, DO, DE] =
      withCreate(Create[I, O, E](inputSchema, successSchema, errorSchema))

    def withCreate[I, O, E](create: Create[I, O, E])
      : ServiceOps[I, O, E, RO, RE, UI, UO, UE, DO, DE] =
      copy(createOperation = Some(create))

    def withRead[O, E](successSchema: BonesSchema[O],
                       errorSchema: BonesSchema[E])
      : ServiceOps[CI, CO, CE, O, E, UI, UO, UE, DO, DE] =
      withRead(Read(successSchema, errorSchema))

    def withRead[O, E](
        read: Read[O, E]): ServiceOps[CI, CO, CE, O, E, UI, UO, UE, DO, DE] =
      copy(readOperation = Some(read))
    def withUpdate[I, O, E](
        inputSchema: BonesSchema[I],
        successSchema: BonesSchema[O],
        errorSchema: BonesSchema[E]
    ): ServiceOps[CI, CO, CE, RO, RE, I, O, E, DO, DE] =
      withUpdate(Update(inputSchema, successSchema, errorSchema))

    def withUpdate[I, O, E](update: Update[I, O, E])
      : ServiceOps[CI, CO, CE, RO, RE, I, O, E, DO, DE] =
      copy(updateOperation = Some(update))

    def withDelete[O, E](
        outputSchema: BonesSchema[O],
        errorSchema: BonesSchema[E]
    ): ServiceOps[CI, CO, CE, RO, RE, UI, UO, UE, O, E] =
      withDelete(Delete(outputSchema, errorSchema))

    def withDelete[O, E](delete: Delete[O, E])
      : ServiceOps[CI, CO, CE, RO, RE, UI, UO, UE, O, E] =
      copy(deleteOperation = Some(delete))
  }

  /**
    *
    * @param inputSchema The expected schema to createOperation new data.
    * @param errorSchema The schema for the error.
    * @param successSchema The for the returned value.
    * @return a GADT describing createOperation
    */
  def create[I, O, E](
      inputSchema: BonesSchema[I],
      successSchema: BonesSchema[O],
      errorSchema: BonesSchema[E]
  ): Create[I, O, E] =
    Create[I, O, E](inputSchema, successSchema, errorSchema)

  /**
    * Use to createOperation a GADT describing how to readOperation data.
    * @param successSchema The data returned on the readOperation.
    * @tparam O the data
    * @return a GADT algebra describing readOperation.
    */
  def read[O, E](successSchema: BonesSchema[O],
                 errorSchema: BonesSchema[E]): Read[O, E] =
    Read(successSchema, errorSchema)

  /**
    * Used to createOperation a GADT describing how to upadate data.
    * @param inputSchema The data the CRUD app is expecting to receive.
    * @param errorSchema The data the CRUD app returns on error.
    * @param successSchema The data the CRUD app returnes on Success.
    * @tparam I Input type.
    * @tparam E Error Type
    * @tparam O Output Type
    * @return a GADT algebra describing updateOperation.
    */
  def update[I, O, E](
      inputSchema: BonesSchema[I],
      successSchema: BonesSchema[O],
      errorSchema: BonesSchema[E]
  ): Update[I, O, E] = Update(inputSchema, successSchema, errorSchema)

  def delete[O, E](
      outputSchema: BonesSchema[O],
      errorSchema: BonesSchema[E]
  ): Delete[O, E] = Delete(outputSchema, errorSchema)

  case class Create[I, O, E](
      inputSchema: BonesSchema[I],
      successSchema: BonesSchema[O],
      errorSchema: BonesSchema[E]
  )

  case class Read[O, E](successSchemaForRead: BonesSchema[O],
                        errorSchema: BonesSchema[E])

  case class Update[I, O, E](
      inputSchema: BonesSchema[I],
      successSchema: BonesSchema[O],
      failureSchema: BonesSchema[E]
  )

  case class Delete[O, E](
      successSchema: BonesSchema[O],
      errorSchema: BonesSchema[E]
  )

  case class Search[O](successSchema: BonesSchema[O])

}
