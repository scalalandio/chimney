# Cheat Sheet

This section is short summary of all Chimney features (described in more detail in other sections).

!!! tip

    We're **strongly** recommend using the search bar (above) to quickly find the relevant piece of documentation.

    Material for MkDocs' search implementation is really good!

## Type classes

 * `Transformer[From, To]` - transforms `From` value into `To` value. The transformation should succeed for all `From`
   values, every `To` field/costructor argument should be populated and every `From` case matched
 * `PartialTransformer[From, To]` - transforms `From` value into `partial.Result[To]` value. By default, the transformation
   should succeed for all `From` values and every `To` field/costructor argument should be populated and every `From` case matched
   (just like `Transformer`) **unless**:
    * the user explicitly provided a constant or a mapping that might fail
    * the user defined/imported implicit `PartialTransformer` for whole/part of the transformation that could fail
   However, even though that provided mappings/values can fail, for derivation to succeed no field can be without a source
   and no pattern match case can be unhandled - *partial* refers to handling: `null`, `Exception`s, smart constructors
   and `PartialFunction`s, not to magically handling the transformation in runtime with no checks.
 * `Patcher[A, Patch]` - updates the `A` value using as a source the `Patch` value.

## Imports

!!! example "DSL, all in one (recommended!)"

    ```scala
    // Summoning:
    // source.transformInto[Target]        summons Transformer.AutoDerived
    // source.transformIntoPartial[Target] summons PartialTransformer.AutoDerived
    // obj.patchUsing(patch)               summons Patcher.AutoDerived
    // Inlined code:
    // source.into[Target].customization.transform        generates inlined Transformer code
    // source.intoPartial[Target].customization.transform generates inlined PartialTransformer code
    // obj.using(patch).customization.patch               generates inlined Patcher code
    import io.scalaland.chimney.dsl._
    ```

!!! example "DSL, selective (use with care!)"

    ```scala
    // Summoning:
    // source.transformInto[Target]        summons Transformer
    // source.transformIntoPartial[Target] summons PartialTransformer
    // obj.patchUsing(patch)               summons Patcher
    import io.scalaland.chimney.syntax._
    // Inlined code:
    // source.into[Target].customization.transform        generates inlined Transformer code
    // source.intoPartial[Target].customization.transform generates inlined PartialTransformer code
    // obj.using(patch).customization.patch               generates inlined Patcher code
    import io.scalaland.chimney.inline._
    // Automatic derivation returns Transformer/PartialTransformer/Patcher
    // instead of Transformer.AutoDerived/PartialTransformer.AutoDerived/Patcher.AutoDerived
    // (see below).
    import io.scalaland.chimney.auto._
    ```

!!! warning

    Chimney uses [sanely-automatic derivation](cookbook.md#automatic-semiautomatic-and-inlined-derivation) - it tries
    to derive instance recursively with a single macro expansion, with is both less taking on typer (faster compilation)
    as well as allows avoiding unnecessary boxing with `partial.Result`.
    
    By importing `io.scalaland.chimney.auto._` we're forcng Chimney to create intermediate instances which breaks
    these improvements (but it might be more familiar behavior for people coming from e.g. Circe).

!!! example "Partial Results"

    ```scala
    // To use partial.Result, partial.Path, partial.Errors, etc without polluting namespace with common names.
    import io.scalaland.chimney.partial
    // Provides .asResult syntax.
    import io.scalaland.chimney.partial.syntax._
    ```

    Partial Results are desctibed more in [`partial.Result` utilities](supported-transformations.md#partialresult-utilities) section.

## Derivation

We're assuming that you used the recommended `io.scalaland.chimney.dsl._` import.

### Total Transformers' derivation

| Syntax                                                              | What it does                                                                                                                                                                                                                                                                       |
|---------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `source.transformInto[Target]`                                      | summons a user-defined `Transformer[Source, Target]`, if there is none, falls back on a `Transformer.AutoDerived[Source, Target]`, then uses it to convert the `source: Source` into the `Target`                                                                                  |
| `source.into[Target].transform`                                     | summons a user-defined `Transformer[Source, Target]` and uses it to convert the `source: Source` into the `Target`, if there is none, generates the inlined conversion (without a `new Transformer`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation) |
| `source.into[Target] .customization.transform`                      | uses provided overrides/flags to generate an inlined conversion from the `source: Source` into the `Target` (without a `new Transformer`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                            |
| `Transformer.derive[Source,Target]`                                 | generates a new instance of `Transformer[Source, Target]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                                                |
| `Transformer.define[Source,Target] .customization.buildTransformer` | uses provided overrides/flags to generate a new instance of `Transformer[Source, Target]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                |

### Partial Transformers' derivation

| Syntex                                                                      | What it does                                                                                                                                                                                                                                                                                                                                                                                |
|-----------------------------------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `source .transformIntoPartial[Target]`                                      | summons a user-defined `PartialTransformer[Source, Target]`, if there is none, falls back on user-defined `Transformer[Source, Target]`, if there is none, falls back on a `PartialTransformer.AutoDerived[Source, Target]`, then uses it to convert the `source: Source` into the `partial.Result[Target]`                                                                                 |
| `source.intoPartial[Target] .transform`                                     | summons a user-defined `PartialTransformer[Source, Target]` and uses it to convert the `source: Source` into the `partial.Result[Target]`, if there is none, falls back on user-defined `Transformer[Source, Target]`,if there is none, generates the inlined conversion (without a `new PartialTransformer`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation) |
| `source.intoPartial[Target] .customization.transform`                       | uses provided overrides/flags to generate an inlined conversion from the `source: Source` into the `partial.Result[Target]` (without a `new PartialTransformer`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                                              |
| `PartialTransformer .derive[Source,Target]`                                 | generates a new instance of `PartialTransformer[Source, Target]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                                                                                                                                                  |
| `PartialTransformer .define[Source,Target] .customization.buildTransformer` | uses provided overrides/flags to generate a new instance of `PartialTransformer[Source, Target]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                                                                                                                  |

### Patchers' derivation

| Syntax                                                | What it does                                                                                                                                                                                                                                                     |
|-------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `obj.patchUsing(patch)`                               | summons a user-defined `Patcher[A, Patch]`, if there is none, falls back on a `Patcher.AutoDerived[A, Patch]`, then uses it to patch the `obj: A` with the `patch: Patch`                                                                                        |
| `obj.using(patch).patch`                              | summons a user-defined `Patcher[A, Patch]` and uses it to patch the `obj: A` with the `patch: Patch`, if there is none, generates the inlined conversion (without a `new Patcher`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation) |
| `obj.using(patch) .customization.patch`               | uses provided overrides/flags to generate a patching of the `obj: A` with the `patch: Patch` (without a `new Patcher`!) - see: [inlined](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                             |
| `Patcher.derive[A,Patch]`                             | generates a new instance of `Patcher[A, Patch]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                                                        |
| `Patcher.define[A,Patch] .customization.buildPatcher` | uses provided overrides/flags to generate a new instance of `Patcher[A, Patch]` - see: [semi](cookbook.md#automatic-semiautomatic-and-inlined-derivation)                                                                                                        |

## Customizations

### Total and Partial Transformers' customizations

All flags and overrides are described in more detail in the [Supported Transformations page](supported-transformations.md).

TODO: flags and overrides

### Patchers' customization

All flags and overrides are described in more detail in the [Supported Patching page](supported-patching.md).

TODO: flags and overrides

## Integrations

Integrations are described in more detail in [Integrations section](cookbook.md#integrations).

### Total Transformers' integrations

!!! example "Providing and using implicit Transformers"

    ```scala
    import io.scalaland.chimney.Transformer

    // Such implicit Transformer would be used if there are no overrides, it cannot cooperate
    // with selectors in DSL (.matching[Type], .matchingSome, .matchingLeft, .matchingRight,
    // .everyItem, .everyMapKey, .everyMapValue, field name selection...), so it works best
    // for types that we either don't intend to customize OR we want to define Transformer only
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
    import io.scalaland.chimney.integrations
    import io.scalaland.chimney.partial

    // Here, we're providing Transformers with the ability to .map F[_]/.traverse F[_] into partial.Result,
    // such impliclit allows us to use .everyItem in customizations of F[value] transformation.
    implicit def outerTransformer[A, B]: integration.TotalOuterTransformer[F[A], F[B], A, B] =
      new integrations.TotalOuterTransformer[F[A], F[B], A, B] {
        /** Converts the outer type when the conversion of inner types turn out to be total. */
        def transformWithTotalInner(inner: F[A], f: A => B): F[B] = ...
        /** Converts the outer type when the conversion of inner types turn out to be partial. */
        def transformWithPartialInner(inner: F[A], failFast: Boolean, f: A => partial.Result[B]): partia.Result[F[B]] = ...
      }

    // Here', we're providing Transformer with the ability to convert into MyOwnCollection[A]
    // from any other collection (whose items can be converted to A), convert from MyOwnCollection[A]
    // (when every item can be converted to the target collection's type), and customize the
    // transformation between the collections using .everyItem.
    implicit def buildIterable[A]: integration.TotallyBuildIterable[MyOwnCollection[A], A] =
      new integration.TotallyBuildIterable[MyOwnCollection[A], A] {
        /** Factory of the `Collection` */
        def totalFactory: Factory[A, MyOwnCollection[A]] = ...
        /** Creates [[Iterator]] for the `Collection`. */
        def iterator(collection: MyOwnCollection[A]): Iterator[A] = ...
      }

    // Like above, but additionally allows working with .everyMapKey/.everyMapValue.
    implicit def buildMap[K, V]: integration.TotallyBuildMap[MyOwnMap[K, V], K, V] =
      new integration.TotallyBuildMap[MyOwnMap[K, V], K, V] {
        /** Factory of the `Collection` */
        def totalFactory: Factory[(K, V), MyOwnMap[K, V]] = ...
        /** Creates [[Iterator]] for the `Collection`. */
        def iterator(collection: MyOwnMap[K, V]): Iterator[(K, V)] = ...
      }

    // Here, we're handling some type representing Option, which is not scala.Option,
    // such implicit allows automatic unwrapping in PartialTransformer/wrapping in every case,
    // and usage of .matchingSome in customization of MyOwnOptional[B] transformation.
    implicit def nonStandardOptional[B]: integrations.OptionalValue[MyOwnOptional[B], B] =
      new integrations.OptionalValue[MyOwnOptional[B], B] {
        /** Creates an empty optional value. */
        def empty: MyOwnOptional[B] = ...
        /** Creates non-empty optional value (should handle nulls as empty). */
        def of(value: B): MyOwnOptional[B] = ...
        /** Folds optional value just like [[Option.fold]]. */
        def fold[A](oa: MyOwnOptional[B], onNone: => A, onSome: B => A): A = ...
      }

    // Here, we're handling the case when we would like to use .enableDefaultValues,
    // or .enableDefaultValueOfType[DefaultValueSet] but the DefaultValueSet type is used
    // by some class that we're transformaing, which does not have default values defined.
    // With such implicit we can pretend it does.
    implicit val providedMissingDefault: integrations.DefaultValue[Value] =
      new integrations.DefaultValue[Value] {
        /** Provide the default value. */
        def provide(): Value = ...
      }
    ```

!!! tip

    All integrations for `Transformer`s work with `PartialTransformer`s and `Patcher`s as well!

### Partial Transformations' integrations

!!! example "Providing and using implicit PartialTransformers"

    ```scala
    import io.scalaland.chimney.PartialTransformer

    // Such implicit Transformer would be used if there are no overrides, it cannot cooperate
    // with selectors in DSL (.matching[Type], .matchingSome, .matchingLeft, .matchingRight,
    // .everyItem, .everyMapKey, .everyMapValue, field name selection...), so it works best
    // for types that we either don't intend to customize OR we want to define Transformer only
    // once and then reuse everywhere.
    implicit val transformerWithHardcodedType: PartialTransformer[A, B] = ...

    // Here, we're using Transformer in another implicit, when Transformer could be either
    // derived or provided - use only when the derivation could not be customized
    // nor can be provided by integrations.
    implicit def transformerWithHardcodedTypes2(
      implicit transformer: PartialTransformer.AutoDerived[C, D] // make sure it's .AutoDerived!
    ): PartialTransformer[E, F] = ...
    ```

!!! example "Providing integrations, more flexible than hardcoded Transformer"

    ```scala
    import io.scalaland.chimney.integrations
    import io.scalaland.chimney.partial

    // Here, we're providing Transformers with the ability to .map F[_]/.traverse F[_] into partial.Result,
    // such impliclit allows us to use .everyItem in customizations of F[value] transformation.
    implicit def outerTransformer[A, B]: integration.PartialOuterTransformer[F[A], F[B], A, B] =
      new integrations.PartialOuterTransformer[F[A], F[B], A, B] {
        /** Converts the outer type when the conversion of inner types turn out to be total. */
        def transformWithTotalInner(inner: F[A], failFast: Boolean,f: A => B): partia.Result[F[B]] = ...
        /** Converts the outer type when the conversion of inner types turn out to be partial. */
        def transformWithPartialInner(inner: F[A], failFast: Boolean, f: A => partial.Result[B]): partia.Result[F[B]] = ...
      }

    // Here', we're providing Transformer with the ability to convert into MyOwnCollection[A]
    // from any other collection (whose items can be converted to A), convert from MyOwnCollection[A]
    // (when every item can be converted to the target collection's type), and customize the
    // transformation between the collections using .everyItem.
    implicit def buildIterable[A]: integration.TotallyBuildIterable[MyOwnCollection[A], A] =
      new integration.TotallyBuildIterable[MyOwnCollection[A], A] {
        /** Factory of the `Collection`, validated with [[partial.Result]]. */
        def partialFactory: Factory[A, partial.Result[MyOwnCollection[A]]] = ...
        /** Creates [[Iterator]] for the `Collection`. */
        def iterator(collection: MyOwnCollection[A]): Iterator[A] = ...
      }

    // Like above, but additionally allows working with .everyMapKey/.everyMapValue.
    implicit def buildMap[K, V]: integration.PartiallyBuildMap[MyOwnMap[K, V], K, V] =
      new integration.PartiallyBuildMap[MyOwnMap[K, V], K, V] {
        /** Factory of the `Collection`, validated with [[partial.Result]]. */
        def partialFactory: Factory[(K, V), partial.Result[MyOwnMap[K, V]]] = ...
        /** Creates [[Iterator]] for the `Collection`. */
        def iterator(collection: MyOwnMap[K, V]): Iterator[(K, V)] = ...
      }
    ```

### Patchers' integrations

!!! example "Providing and using implicit Patcher"

    ```scala
    import io.scalaland.chimney.Patcher

    // Such implicit Patcher would be used if there are no overrides, it cannot cooperate
    // with selectors in DSL (.matching[Type], .matchingSome, .matchingLeft, .matchingRight,
    // .everyItem, .everyMapKey, .everyMapValue, field name selection...), so it works best
    // for types that we either don't intend to customize OR we want to define Patcher only
    // once and then reuse everywhere.
    implicit val patcherWithHardcodedType: Patcher[A, B] = ...

    // Here, we're using Patcher in another implicit, when Patcher could be either
    // derived or provided - use only when the derivation could not be customized
    // nor can be provided by integrations.
    implicit def patcherWithHardcodedTypes2(
      implicit patcher: Patcher.AutoDerived[C, D] // make sure it's .AutoDerived!
    ): Patcher[E, F] = ...
    ```
