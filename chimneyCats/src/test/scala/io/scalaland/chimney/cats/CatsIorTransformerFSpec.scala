package io.scalaland.chimney.cats

import cats.data.{Ior, IorNec, IorNel, IorNes, NonEmptyChain}
import io.scalaland.chimney.examples.trip.{Person, PersonForm, User}
import utest._
import io.scalaland.chimney.dsl._
import io.scalaland.chimney.utils.OptionUtils._

object CatsIorTransformerFSpec extends TestSuite {
  val tests: Tests = Tests {
    "transform always becomes a Right" - {
      Person("John", 10, 140).intoF[IorNec[String, +*], User].transform ==> Ior.right(User("John", 10, 140))
      Person("John", 10, 140).intoF[IorNel[String, +*], User].transform ==> Ior.right(User("John", 10, 140))
      Person("John", 10, 140).intoF[IorNes[String, +*], User].transform ==> Ior.right(User("John", 10, 140))
    }

    "transform will result in a Both if a field ends up as a Both" - {
      Person("John", 10, 140)
        .intoF[IorNec[String, +*], User]
        .withFieldConstF(_.name, Ior.both(NonEmptyChain("Name should not have dots in it"), "John.Doe"))
        .transform ==> Ior.both(NonEmptyChain("Name should not have dots in it"), User("John.Doe", 10, 140))
    }

    "transform will result in a Left if a field ends up as a Left" - {
      Person("", 10, 140)
        .intoF[IorNec[String, +*], User]
        .withFieldConstF(_.name, Ior.left(NonEmptyChain("You must provide a name")))
        .transform ==> Ior.left(NonEmptyChain("You must provide a name"))
    }

    "simple transforms with Ior" - {
      "success" - {
        val okForm = PersonForm("John", "10", "140")

        "1-arg" - {
          okForm
            .into[Person]
            .withFieldConst(_.height, 200.5)
            .withFieldComputedF[IorNec[String, +*], Int, Int](
              _.age,
              _.age.parseInt.map(Ior.right).getOrElse(Ior.left(NonEmptyChain("Invalid age entered")))
            )
            .transform ==> Ior.right(Person("John", 10, 200.5))
        }

        "2-arg (accumulates errors)" - {
          okForm
            .intoF[IorNec[String, +*], Person]
            .withFieldComputedF(_.age, _ => Ior.both(NonEmptyChain("age warning"), 10))
            .withFieldComputedF(_.height, _ => Ior.both(NonEmptyChain("height warning"), 100.0))
            .transform ==> Ior.both(NonEmptyChain("age warning", "height warning"), Person("John", 10, 100.0))
        }

        "3-arg (accumulate errors to the first Left" - {
          okForm
            .intoF[IorNec[String, +*], Person]
            .withFieldComputedF(
              _.name,
              _ => Ior.both(NonEmptyChain("Putting a dot in the name is deprecated"), "John.Doe")
            )
            .withFieldConstF(_.age, Ior.left(NonEmptyChain("age is too low")))
            .withFieldConstF(_.height, Ior.both(NonEmptyChain("height not available, using default"), 10.0))
            .transform ==> Ior.left(NonEmptyChain("Putting a dot in the name is deprecated", "age is too low"))
        }
      }
    }

    "traverse should accumulate on the left side" - {
      TransformerFIorSupport[NonEmptyChain[String]]
        .traverse(
          Iterator("bla", "ha", "hee", "bee"),
          (input: String) => Ior.both(NonEmptyChain(s"Accumulating $input"), input)
        )
        .map(_.toList) ==>
        Ior.both(
          NonEmptyChain("Accumulating bla", "Accumulating ha", "Accumulating hee", "Accumulating bee"),
          List("bla", "ha", "hee", "bee")
        )
    }

    "wrapped subtype transformation" - {
      class Foo(val x: Int)
      case class Bar(override val x: Int) extends Foo(x)

      Bar(100).intoF[IorNec[String, +*], Foo].transform.right.map(_.x) ==> Some(100)
    }
  }
}
