package com.bones

package object doobie {

  case class WithId[P](id: Long, p: P)

}
