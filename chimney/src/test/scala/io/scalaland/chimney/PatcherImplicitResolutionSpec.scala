package io.scalaland.chimney

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.fixtures.*

class PatcherImplicitResolutionSpec extends ChimneySpec {

  test("patch using implicit Patcher for whole patching when available") {
    import PatchDomain.*

    case class PhonePatch(phone: Option[Phone])

    locally {
      implicit val instance: Patcher[UserWithOptionalField, PhonePatch] =
        Patcher.define[UserWithOptionalField, PhonePatch].ignoreNoneInPatch.buildPatcher

      exampleUserWithOptionalField.patchUsing(PhonePatch(None)) ==> exampleUserWithOptionalField
      exampleUserWithOptionalField.using(PhonePatch(None)).patch ==> exampleUserWithOptionalField
    }

    locally {
      implicit val cfg = PatcherConfiguration.default.ignoreNoneInPatch

      implicit val instance: Patcher[UserWithOptionalField, PhonePatch] =
        Patcher.derive[UserWithOptionalField, PhonePatch]

      exampleUserWithOptionalField.patchUsing(PhonePatch(None)) ==> exampleUserWithOptionalField
      exampleUserWithOptionalField.using(PhonePatch(None)).patch ==> exampleUserWithOptionalField
    }
  }
}
