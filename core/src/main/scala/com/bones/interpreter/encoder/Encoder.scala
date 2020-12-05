package com.bones.interpreter.encoder

trait Encoder[ALG[_], A, OUT] { self =>
  def encode(a: A): OUT

  def map[OUTB](f: OUT => OUTB): Encoder[ALG, A, OUTB] = (a: A) => f(self.encode(a))

}
