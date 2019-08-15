package com.bones.crud

import com.bones.data.Value.BonesSchema

/**
  * Defines the algebra for the CRUD actions.
  */
object Algebra {

  object ServiceOps {
    def havingPath(path: String) = ServiceOps(path, None, None, None, None)

    /**
      * Creates service ops having the same input and output like most
      * basic CRUD (Create Read Update Delete) operations.  Note that the interpreter can choose to add
      * data such as an ID on output.
      *
      * @param path        The root path for the crud operations.
      * @param schema      The basic schema for all input/output data.
      * @param errorSchema The expected schema for any errors reproted.
      * @tparam A the CRUD data type
      * @tparam E The error date type.
      * @return ServicesOps describing your classic CRUD application.
      */
    def classicCrud[A, E](path: String,
                          schema: BonesSchema[A],
                          errorSchema: BonesSchema[E]): ServiceOps[A, A, E, A, E, A, A, E, A, E] = {
      ServiceOps
        .havingPath(path)
        .providingCreate(schema, schema, errorSchema)
        .providingRead(schema, errorSchema)
        .providingUpdate(schema, schema, errorSchema)
        .providingDelete(schema, errorSchema)
    }
  }

  /** Collection each of the Crud operation descriptions for a service */
  case class ServiceOps[CI, CO, CE, RO, RE, UI, UO, UE, DO, DE](
                                                                 path: String,
                                                                 createOperation: Option[Create[CI, CO, CE]],
                                                                 readOperation: Option[Read[RO, RE]],
                                                                 updateOperation: Option[Update[UI, UO, UE]],
                                                                 deleteOperation: Option[Delete[DO, DE]]
                                                               ) {
    def providingCreate[I, O, E](
                                  inputSchema: BonesSchema[I],
                                  outputSchema: BonesSchema[O],
                                  errorSchema: BonesSchema[E]
                                ): ServiceOps[I, O, E, RO, RE, UI, UO, UE, DO, DE] =
      withCreate(Create[I, O, E](inputSchema, outputSchema, errorSchema))

    def withCreate[I, O, E](create: Create[I, O, E])
    : ServiceOps[I, O, E, RO, RE, UI, UO, UE, DO, DE] =
      copy(createOperation = Some(create))

    def providingRead[O, E](outputSchema: BonesSchema[O],
                            errorSchema: BonesSchema[E])
    : ServiceOps[CI, CO, CE, O, E, UI, UO, UE, DO, DE] =
      providingRead(Read(outputSchema, errorSchema))

    def providingRead[O, E](read: Read[O, E]): ServiceOps[CI, CO, CE, O, E, UI, UO, UE, DO, DE] =
      copy(readOperation = Some(read))

    def providingUpdate[I, O, E](
                                  inputSchema: BonesSchema[I],
                                  outputSchema: BonesSchema[O],
                                  errorSchema: BonesSchema[E]
                                ): ServiceOps[CI, CO, CE, RO, RE, I, O, E, DO, DE] =
      providingUpdate(Update(inputSchema, outputSchema, errorSchema))

    def providingUpdate[I, O, E](update: Update[I, O, E])
    : ServiceOps[CI, CO, CE, RO, RE, I, O, E, DO, DE] =
      copy(updateOperation = Some(update))

    def providingDelete[O, E](
                               outputSchema: BonesSchema[O],
                               errorSchema: BonesSchema[E]
                             ): ServiceOps[CI, CO, CE, RO, RE, UI, UO, UE, O, E] =
      providingDelete(Delete(outputSchema, errorSchema))

    def providingDelete[O, E](delete: Delete[O, E])
    : ServiceOps[CI, CO, CE, RO, RE, UI, UO, UE, O, E] =
      copy(deleteOperation = Some(delete))
  }

  /**
    *
    * @param inputSchema  The expected schema to createOperation new data.
    * @param errorSchema  The schema for the error.
    * @param outputSchema The for the returned value.
    * @return a GADT describing createOperation
    */
  def create[I, O, E](
                       inputSchema: BonesSchema[I],
                       outputSchema: BonesSchema[O],
                       errorSchema: BonesSchema[E]
                     ): Create[I, O, E] =
    Create[I, O, E](inputSchema, outputSchema, errorSchema)

  /**
    * Use to createOperation a GADT describing how to readOperation data.
    *
    * @param outputSchema The data returned on the readOperation.
    * @tparam O the data
    * @return a GADT algebra describing readOperation.
    */
  def read[O, E](outputSchema: BonesSchema[O],
                 errorSchema: BonesSchema[E]): Read[O, E] =
    Read(outputSchema, errorSchema)

  /**
    * Used to createOperation a GADT describing how to upadate data.
    *
    * @param inputSchema  The data the CRUD app is expecting to receive.
    * @param errorSchema  The data the CRUD app returns on error.
    * @param outputSchema The data the CRUD app returnes on Success.
    * @tparam I Input type.
    * @tparam E Error Type
    * @tparam O Output Type
    * @return a GADT algebra describing updateOperation.
    */
  def update[I, O, E](
                       inputSchema: BonesSchema[I],
                       outputSchema: BonesSchema[O],
                       errorSchema: BonesSchema[E]
                     ): Update[I, O, E] = Update(inputSchema, outputSchema, errorSchema)

  def delete[O, E](
                    outputSchema: BonesSchema[O],
                    errorSchema: BonesSchema[E]
                  ): Delete[O, E] = Delete(outputSchema, errorSchema)


  abstract class CrudLayer[I,O]

  /**
    * Create describes an operation which takes the input I and creates the value O or reporting E if in error.
    *
    * @param inputSchema  The schema definition used for Input.
    * @param outputSchema The schema definition used for Output.
    * @param errorSchema  The schema definition used for the error.
    * @tparam I Input type
    * @tparam O Output Type
    * @tparam E Error Type
    */
  case class Create[I, O, E](
                              inputSchema: BonesSchema[I],
                              outputSchema: BonesSchema[O],
                              errorSchema: BonesSchema[E]
                            ) extends CrudLayer[I,Either[E,O]]

  /**
    * Read describes an operation where O is generated or E is reported as an error
    *
    * @param outputSchema The expected type of the Generated O
    * @param errorSchema  The type for the error type E
    * @tparam O Output Type
    * @tparam E Error Type
    */
  case class Read[O, E](outputSchema: BonesSchema[O],
                        errorSchema: BonesSchema[E]) extends CrudLayer[Long,Either[E,O]]

  /** Update describes an operation to update an entity where I is the input type,
    * O is the output type and E is reported in error.
    *
    * @param inputSchema   The schema describing the input data.
    * @param outputSchema  The schema describing the success/output data.
    * @param failureSchema The schema describing the error state.
    * @tparam I Input type
    * @tparam O Output type
    * @tparam E Error type
    */
  case class Update[I, O, E](
                              inputSchema: BonesSchema[I],
                              outputSchema: BonesSchema[O],
                              failureSchema: BonesSchema[E]
                            ) extends CrudLayer[I,Either[E,O]]

  case class Delete[O, E](
                           outputSchema: BonesSchema[O],
                           errorSchema: BonesSchema[E]
                         ) extends CrudLayer[Long,Either[E,O]]

  case class Search[I,O](i: I, outputSchema: BonesSchema[O]) extends CrudLayer[I,Seq[O]]

}
