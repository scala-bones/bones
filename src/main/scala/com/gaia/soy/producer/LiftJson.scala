package com.gaia.soy.producer

import com.gaia.soy
import com.gaia.soy.JsonProducer

case class LiftJson() extends JsonProducer {


  override def produceBigDecimal(key: soy.Key) = ???

  override def produceString(key: soy.Key) = ???

  override def produceInt(key: soy.Key) = ???

  override def produceObject(key: soy.Key) = ???

  override def produceBool(key: soy.Key) = ???
}
