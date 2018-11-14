package com.bones

import com.bones.data.Value.KvpNil
import com.bones.syntax._

object Strangeloop {


  val dataDefinition =
    kvp("id", int) ::
    kvp("name", string) ::
    kvp("presentationName", string) ::
    kvp("presentationDate", isoDateTime) ::
    KvpNil



}
