package io.scalaland.chimney.internal

import io.scalaland.chimney.examples
import org.scalatest.{MustMatchers, WordSpec}
import shapeless.HNil
import shapeless.syntax.singleton._

class CoproductInstanceProviderSpec extends WordSpec with MustMatchers {

  "CoproductInstanceProvider" should {

    "provide instance for coproducts by matching label of singleton type" in {

      import examples._

      CoproductInstanceProvider
        .provide('Red ->> colors1.Red, classOf[colors2.Color], HNil) mustBe
        colors2.Red
    }

    "respect specific instances overriding" in {

      import examples._

      val overrideModifier = Modifier.coproductInstance { (_: colors1.Red.type) =>
        colors2.Black: colors2.Color
      }

      CoproductInstanceProvider
        .provide('Red ->> colors1.Red, classOf[colors2.Color], overrideModifier :: HNil) mustBe
        colors2.Black
    }

    "respect general transformation overriding" in {

      import examples._

      val overrideModifier = Modifier.coproductInstance[colors1.Color, colors2.Color] { _ =>
        colors2.Blue
      }

      CoproductInstanceProvider
        .provide('Red ->> colors1.Red, classOf[colors2.Color], overrideModifier :: HNil) mustBe
        colors2.Blue
    }
  }
}
