package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

import scala.annotation.unused

class PartialTransformerIntegrationsSpec extends ChimneySpec {

  import TotalTransformerIntegrationsSpec.*

  // TODO: transform from Option-type into Option-type, using Total Transformer for inner type transformation

  // TODO: transform from non-Option-type into Option-type, using Partial Transformer for inner type transformation

  // TODO: transform from non-Option-type into Option-type, using Total Transformer for inner type transformation

  // TODO: transform from non-Option-type into Option-type, using Partial Transformer for inner type transformation

  // TODO: transform from Option-type into non-Option-type, using Total Transformer for inner type transformation

  // TODO: transform from Option-type into non-Option-type, using Partial Transformer for inner type transformation

  // TODO: transform Iterable-type to Iterable-type, using Total Transformer for inner type transformation

  // TODO: transform Iterable-type to Iterable-type, using Partial Transformer for inner type transformation

  // TODO: transform between Array-type and Iterable-type, using Total Transformer for inner type transformation

  // TODO: transform between Array-type and Iterable-type, using Partial Transformer for inner type transformation

  // TODO: transform into sequential type with an override

  // TODO: transform Map-type to Map-type, using Total Transformer for inner type transformation

  // TODO: transform Map-type to Map-type, using Partial Transformer for inner type transformation

  // TODO: transform between Iterables and Maps, using Total Transformer for inner type transformation

  // TODO: transform between Iterables and Maps, using Partial Transformer for inner type transformation

  // TODO: transform between Arrays and Maps, using Total Transformer for inner type transformation

  // TODO: transform between Arrays and Maps, using Partial Transformer for inner type transformation

  // TODO: transform into map type with an override

  group("flag .enableOptionDefaultsToNone") {

    case class Source(x: String)
    case class TargetWithOption(x: String, y: Possible[Int])
    case class TargetWithOptionAndDefault(x: String, y: Possible[Int] = Possible.Present(42))

    test("should be turned off by default and not allow compiling OptionalValue fields with missing source") {
      compileErrorsFixed("""Source("foo").transformIntoPartial[TargetWithOption]""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
      compileErrorsFixed("""Source("foo").intoPartial[TargetWithOption].transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }

    test("should use OptionalValue.empty for fields without source nor default value when enabled") {
      Source("foo").intoPartial[TargetWithOption].enableOptionDefaultsToNone.transform.asOption ==> Some(
        TargetWithOption("foo", Possible.Nope)
      )
      locally {
        implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone
        Source("foo").transformIntoPartial[TargetWithOption].asOption ==> Some(TargetWithOption("foo", Possible.Nope))
      }
    }

    test(
      "should use OptionalValue.empty for fields without source but with default value when enabled but default values disabled"
    ) {
      Source("foo").intoPartial[TargetWithOptionAndDefault].enableOptionDefaultsToNone.transform.asOption ==> Some(
        TargetWithOptionAndDefault("foo", Possible.Nope)
      )
      locally {
        implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

        Source("foo").transformIntoPartial[TargetWithOptionAndDefault].asOption ==> Some(
          TargetWithOptionAndDefault("foo", Possible.Nope)
        )
      }
    }

    test("should be ignored when default value is set and default values enabled") {
      Source("foo")
        .intoPartial[TargetWithOption]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform
        .asOption ==> Some(
        TargetWithOption("foo", Possible.Nope)
      )
      Source("foo")
        .intoPartial[TargetWithOptionAndDefault]
        .enableDefaultValues
        .enableOptionDefaultsToNone
        .transform
        .asOption ==> Some(
        TargetWithOptionAndDefault(
          "foo",
          Possible.Present(42)
        )
      )
    }
  }

  group("flag .disableOptionDefaultsToNone") {

    @unused case class Source(x: String)
    @unused case class TargetWithOption(x: String, y: Possible[Int])

    test("should disable globally enabled .enableOptionDefaultsToNone") {
      @unused implicit val config = TransformerConfiguration.default.enableOptionDefaultsToNone

      compileErrorsFixed("""Source("foo").intoPartial[TargetWithOption].disableOptionDefaultsToNone.transform""").check(
        "Chimney can't derive transformation from io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source to io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "io.scalaland.chimney.PartialTransformerIntegrationsSpec.TargetWithOption",
        "y: io.scalaland.chimney.TotalTransformerIntegrationsSpec.Possible[scala.Int] - no accessor named y in source type io.scalaland.chimney.PartialTransformerIntegrationsSpec.Source",
        "Consult https://chimney.readthedocs.io for usage examples."
      )
    }
  }

  // TODO: flag .enablePartialUnwrapsOption

  // TODO: flag .disablePartialUnwrapsOption
}
