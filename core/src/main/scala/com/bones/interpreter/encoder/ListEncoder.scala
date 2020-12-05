package com.bones.interpreter.encoder

case class ListEncoder[ALG[_], A, OUT](
  singleValueEncoder: Encoder[ALG, A, OUT],
  f: List[OUT] => OUT
) extends Encoder[ALG, List[A], OUT] {
  override def encode(a: List[A]): OUT = {
    val listOfOut = a.map(singleValueEncoder.encode)
    f(listOfOut)
  }
}
