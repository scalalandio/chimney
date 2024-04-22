package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

import scala.annotation.unused

class TotalTransformerIntegrationsSpec extends ChimneySpec {

  import TotalTransformerIntegrationsSpec.*

  // TODO: transform from Option-type into Option-type

  // TODO: transform from non-Option-type into Option-type

  // TODO: transform from Iterable-type to Iterable-type

  // TODO: transform between Array-type and Iterable-type

  // TODO: transform into sequential type with an override

  // TODO transform into map type with an override

  group("flag .enableOptionDefaultsToNone") {

    case class Source(x: String)
    case class TargetWithOption(x: String, y: Possible[Int])
    case class TargetWithOptionAndDefault(x: String, y: Possible[Int] = Possible.Present(42))

    test("should be turned off by default and not allow compiling OptionalValue fields with missing source") {
      compileErrorsFixed("""Source("foo").into[TargetWithOption].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source to io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("use OptionalValue.empty for fields without source nor default value when enabled") {
      Source("foo").into[TargetWithOption].enableOptionDefaultsToNone.transform ==> TargetWithOption(
        "foo",
        Possible.Nope
      )
    }

    test(
      "use OptionalValue.empty for fields without source but with default value when enabled but default values disabled"
    ) {
      Source("foo")
        .into[TargetWithOptionAndDefault]
        .enableOptionDefaultsToNone
        .transform ==> TargetWithOptionAndDefault("foo", Possible.Nope)
    }

    test("should be ignored when default value is set and default values enabled") {
      Source("foo")
        .into[TargetWithOption]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform ==> TargetWithOption("foo", Possible.Nope)
      Source("foo")
        .into[TargetWithOptionAndDefault]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform ==> TargetWithOptionAndDefault(
        "foo",
        Possible.Present(42)
      )
    }
  }

  group("flag .disableOptionDefaultsToNone") {

    @unused case class Source(x: String)
    @unused case class TargetWithOption(x: String, y: Possible[Int])

    test("should disable globally enabled .enableOptionDefaultsToNone") {
      @unused implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

      compileErrorsFixed("""Source("foo").into[TargetWithOption].disableOptionDefaultsToNone.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source to io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.TotalTransformerIntegrationsSpec.TargetWithOption",
        "y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.TotalTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }
}
object TotalTransformerIntegrationsSpec {

  import integrations.*

  sealed trait Possible[+A] extends Product with Serializable
  object Possible {
    case class Present[+A](a: A) extends Possible[A]
    case object Nope extends Possible[Nothing]
  }

  implicit def possibleIsOptionalValue[A]: OptionalValue[Possible[A], A] = new OptionalValue[Possible[A], A] {
    override def empty: Possible[A] = Possible.Nope
    override def of(value: A): Possible[A] = Possible.Present(value)
    override def fold[A0](oa: Possible[A], onNone: => A0, onSome: A => A0): A0 = oa match {
      case Possible.Present(value) => onSome(value)
      case Possible.Nope           => onNone
    }
  }
}
