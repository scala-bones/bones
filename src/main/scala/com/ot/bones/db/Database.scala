package com.ot.bones.db

import com.ot.bones.ProgramModuleOp
import com.ot.bones.validation.DataDefinitionOp

trait DatabaseSyntax {

  import Database._

  def save[T](dataDefinitionOp: DataDefinitionOp[T]): Save[T] = Save(dataDefinitionOp)




}


object Database {


  case class Save[T](dataDefinitionOp: DataDefinitionOp[T]) extends ProgramModuleOp[T => Unit]

}
