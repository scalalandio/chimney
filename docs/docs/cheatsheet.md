# Cheat Sheet

This section is shows all features of Chimney described in more detail in other sections.

## Imports

!!! example "DSL, all in one (recommended!)"

    ```scala
    import io.scalaland.chimney.dsl._
    ```

!!! example "DSL, selective (use with care!)"

    ```scala
    import io.scalaland.chimney.auto._
    import io.scalaland.chimney.inline._
    import io.scalaland.chimney.syntax._
    ```

!!! warning

    TODO: warning about auto in Chimney

!!! example "Partial Results"

    ```scala
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.partial.syntax._
    ```

## Derivation

### Total Transformers' derivation

| code                                                                | meaning                                                                                                                                                                                                                                                                            |
|---------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `source.transformInto[Target]`                                      | summons a user-defined `Transformer[Source, Target]`, if there is none, falls back on a `Transformer.AutoDerived[Source, Target]`, then uses it to convert the `source: Source` into the `Target`                                                                                  |
| `source.into[Target].transform`                                     | summons a user-defined `Transformer[Source, Target]` and uses it to convert the `source: Source` into the `Target`, if there is none, generates the inlined conversion (without a `new Transformer`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation) |
| `source.into[Target].customization.transform`                       | uses provided overrides/flags to generate an inlined conversion from the `source: Source` into the `Target` (without a `new Transformer`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                            |
| `Transformer.derive[Source, Target]`                                | generates a new instance of `Transformer[Source, Target]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                                                |
| `Transformer.define[Source, Target].customization.buildTransformer` | uses provided overrides/flags to generate a new instance of `Transformer[Source, Target]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                |

### Partial Transformers' derivation

| code                                                                       | meaning                                                                                                                                                                                                                                                                                                                                                                                     |
|----------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `source.transformIntoPartial[Target]`                                      | summons a user-defined `PartialTransformer[Source, Target]`, if there is none, falls back on user-defined `Transformer[Source, Target]`, if there is none, falls back on a `PartialTransformer.AutoDerived[Source, Target]`, then uses it to convert the `source: Source` into the `partial.Result[Target]`                                                                                 |
| `source.intoPartial[Target].transform`                                     | summons a user-defined `PartialTransformer[Source, Target]` and uses it to convert the `source: Source` into the `partial.Result[Target]`, if there is none, falls back on user-defined `Transformer[Source, Target]`,if there is none, generates the inlined conversion (without a `new PartialTransformer`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation) |
| `source.intoPartial[Target].customization.transform`                       | uses provided overrides/flags to generate an inlined conversion from the `source: Source` into the `partial.Result[Target]` (without a `new PartialTransformer`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                                              |
| `PartialTransformer.derive[Source, Target]`                                | generates a new instance of `PartialTransformer[Source, Target]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                                                                                                                                                  |
| `PartialTransformer.define[Source, Target].customization.buildTransformer` | uses provided overrides/flags to generate a new instance of `PartialTransformer[Source, Target]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                                                                                                                  |

### Patchers' derivation

| code                                                  | meaning                                                                                                                                                                                                                                                          |
|-------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `obj.patchUsing(patch)`                               | summons a user-defined `Patcher[A, Patch]`, if there is none, falls back on a `Patcher.AutoDerived[A, Patch]`, then uses it to patch the `obj: A` with the `patch: Patch`                                                                                        |
| `obj.using(patch).patch`                              | summons a user-defined `Patcher[A, Patch]` and uses it to patch the `obj: A` with the `patch: Patch`, if there is none, generates the inlined conversion (without a `new Patcher`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation) |
| `obj.using(patch).customization.patch`                | uses provided overrides/flags to generate a patching of the `obj: A` with the `patch: Patch` (without a `new Patcher`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                             |
| `Patcher.derive[A, Patch]`                            | generates a new instance of `Patcher[A, Patch]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                                        |
| `Patcher.define[A, Patch].customization.buildPatcher` | uses provided overrides/flags to generate a new instance of `Patcher[A, Patch]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                        |

## Customizations

## Integrations

### Total Transformers' integrations

!!! example "Providing and using implicit Transformers"

    ```scala
    import io.scalaland.chimney.Transformer

    // Such implicit Transformer would be used if there are no overrides, it cannot cooperate
    // with selectors in DSL (.matching[Type], .matchingSome, .matchingLeft, .matchingRight,
    // .everyItem, .everyMapKey, .everyMapValue, field name selection...), so it works best
    // for types that we either don't intent to customize OR we want to define Transformer only
    // once and then reuse everywhere.
    implicit val transformerWithHardcodedType: Transformer[A, B] = ...

    // Here, we're using Transformer in another implicit, when Transformer could be either
    // derived or provided - use only when the derivation could not be customized
    // nor can be provided by integrations.
    implicit def transformerWithHardcodedTypes2(
      implicit transformer: Transformer.AutoDerived[C, D] // make sure it's .AutoDerived!
    ): Transformer[E, F] = ...
    ```

!!! example "Providing integrations, more flexible than hardcoded Transformer"

    ```scala
    import io.scalaland.chimney.integrations.TotalOuterTransformer
    import io.scalaland.chimney.partial

    // Here we're providing Transformers with the ability to .map F[_]/.traverse F[_] into partial.Result,
    // such impliclit allows us to use .everyItem in customizations of F[value] transformation.
    implicit def outerTransformer[A, B]: integration.TotalOuterTransformer[F[A], F[B]] =
      new integrations.TotalOuterTransformer[F[A], F[B]] {
        def transformWithTotalInner(inner: F[A], f: A => B): F[B] = ...
        def transformWithPartialInner(inner: F[A], f: A => partial.Result[B]): partia.Result[F[B]] = ...
      }
    ```

TODO: TotallyBuildIterable

### Partial Transformations' integrations

TODO

### Patchers' integrations

TODO
