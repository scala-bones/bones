package com.bones

import com.bones.data.Value.KvpNil
import com.bones.syntax.key

object Strangeloop {


  val dataDefinition =
    key("id").int() ::
    key("name").string() ::
    key("presentationName").string() ::
    key("presentationDate").isoDateTime() ::
    KvpNil



}
