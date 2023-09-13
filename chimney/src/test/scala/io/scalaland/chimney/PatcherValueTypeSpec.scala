package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PatcherValueTypeSpec extends ChimneySpec {

  test("patch an object containing value classes in a patch") {

    import PatchDomain.*

    val update = UpdateDetails("xyz@def.com", 123123123L)

    exampleUser.patchUsing(update) ==> User(10, Email("xyz@def.com"), Phone(123123123L))
    exampleUser.using(update).patch ==> User(10, Email("xyz@def.com"), Phone(123123123L))
  }
}
