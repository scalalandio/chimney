# Cheat Sheet

This section is short summary of all Chimney features (described in more detail in other sections).

!!! tip

    We're **strongly** recommend using the search bar (above) to quickly find the relevant piece of documentation.

    Material for MkDocs' search implementation is really good!

## Type classes

 * `Transformer[From, To]` - transforms `From` value into `To` value. The transformation should succeed for all `From`
   values, every `To` field/constructor argument should be populated and every `From` case matched
 * `PartialTransformer[From, To]` - transforms `From` value into `partial.Result[To]` value. By default, the transformation
   should succeed for all `From` values and every `To` field/constructor argument should be populated and every `From` case matched
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
    //
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

Out of the box, Chimney is able to generate transformations between: `case class`es, `sealed` hierarchies/Scala 3 `enum`s/Java `enum`s,
`Option`s, `Either`s, collections, unwrapping/wrapping/rewrapping `AnyVal`s, etc.

If it misses some information, derivation would report an informative error to the users, to help them fix it with a customization.

This is the most common way users would use Chimney.

!!! note

    We're assuming that you imported the recommended `io.scalaland.chimney.dsl._` (and `Transformer`, `PartialTransformer` and `Patcher` for `.derive`/`.define`).

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

When out of the box derivation doesn't fit ones needs, they can provide overrides and flags to adjust it.

For types supported out of the box, these are usually enough to allow Chimney generate the transformation.

### Total and Partial Transformers' customizations

All flags and overrides are described in more detail in the [Supported Transformations page](supported-transformations.md).

!!! note

    Examples below assume:

     *  transformation `From` into `To`
     * `fromField: FromField`
     * `toField: ToField`
     * `FromSubtype <: From`
     * `ToSubtype <: To`

    for convention.

    While they show only path selectors like `.toField`/`.fromField`/`.matchingSome`/`.everyItem`, you can
    also use `.matching[Subtype]`/`.matchingLeft`/`.matchingRight`/`.everyMapKey`/`.everyMapValue`, combine them
    together, etc. These examples aren't an exhaustive list but just show what is possible.

    You can chain multiple overrides together.

??? tip "How to remember which override to use"

    Override names are constructed in the following way:

     * they start with the target of on override:
        * `withConstructor` - instead of using the primary contructor, provided constructor should be used
        * `withFallback` - when the source value is missing a field, this value would be used next
        * `withField` - instead of using the source field matched by name/position (in case of tuples), this value/function should be used
        * `withSealedSubtype`/`withEnumCase` - instead of using the target subtype matched by name, this function should be used
     * then, some of them reflect how override is provided:
        * `Const` - value is provided as it is
        * `Computed` - value would be computed from the source or its part
        * `Handled` - only a subtype will be handled, with the provided function
     * then, if the override can result in a failed value, name contain `Partial`
     * finally, if the override does not target a whole `Source`/`Target` type, it describes the path to value:
        * `From` - when only some part of `Source` type would be used
        * `To` - when only part of the `Target` type would be created

| Override example                                                                                    | What it does                                                                                                                                                                |
|-----------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `.withConstructor { (args) => ... }`                                                                | use the [provided function to construct `To`](supported-transformations.md#types-with-manually-provided-constructors) (wiring arguments)                                    |
| `.withConstructorTo(_.toField) { (args) => ... }`                                                   | use the [provided function to construct `ToField`](supported-transformations.md#types-with-manually-provided-constructors) (wiring arguments)                               |
| `.withConstructorTo(_.matchingSome.toField) { (args) => ... }`                                      | the same as above (but field is in `Option`)                                                                                                                                |
| `.withConstructorTo(_.everyItem.toField) { (args) => ... }`                                         | the same as above (but field is in collection)                                                                                                                              |
| `.withConstructorPartial { (args) => ... }`                                                         | use the [provided function to construct `partial.Result[To]`](supported-transformations.md#types-with-manually-provided-constructors)                                       |
| `.withConstructorPartialTo(_.toField) { (args) => ... }`                                            | use the [provided function to construct `partial.Result[ToField]`](supported-transformations.md#types-with-manually-provided-constructors)                                  |
| `.withConstructorPartialTo(_.matchingSome.toField) { (args) => ... }`                               | the same as above (but field is in `Option`)                                                                                                                                |
| `.withConstructorPartialTo(_.matchingSome.toField) { (args) => ... }`                               | the same as above (but field is in collection)                                                                                                                              |
| `.withConstructorEither { (args) => ... }`                                                          | use the [provided function to construct `Either[String, To]`](supported-transformations.md#types-with-manually-provided-constructors)                                       |
| `.withConstructorEitherTo(_.toField) { (args) => ... }`                                             | use the [provided function to construct `Either[String, ToField]`](supported-transformations.md#types-with-manually-provided-constructors)                                  |
| `.withConstructorEitherTo(_.matchingSome.toField) { (args) => ... }`                                | the same as above (but a field is in `Option`)                                                                                                                              |
| `.withConstructorEitherTo(_.everyItem.toField) { (args) => ... }`                                   | the same as above (but a field is in collection)                                                                                                                            |
| `.withFallback(fallback)`                                                                           | when `From` is missing values, [they will be taken from `fallback`](supported-transformations.md#merging-multiple-input-sources-into-a-single-target-value)                 |
| `.withFallbackFrom(_.fromField)(fallback)`                                                          | when `FromField` is missing values, [they will be taken from `fallback`](supported-transformations.md#merging-multiple-input-sources-into-a-single-target-value)            |
| `.withFallbackFrom(_.matchingSome)(fallback)`                                                       | the same as above (but a field is in `Option`)                                                                                                                              |
| `.withFallbackFrom(_.everyItem)(fallback)`                                                          | the same as above (but a field is in collection)                                                                                                                            |
| `.withFieldConst(_.toField, value)`                                                                 | use the [provided value to construct `toField`](supported-transformations.md#wiring-the-constructors-parameter-to-a-provided-value)                                         |
| `.withFieldConst(_.matchingSome.toField, value)`                                                    | the same as above (but a field is in `Option`)                                                                                                                              |
| `.withFieldConst(_.everyItem.toField, value)`                                                       | the same as above (but a field is in collection)                                                                                                                            |
| `.withFieldConstPartial(_.toField, value)`                                                          | use the [provided `partial.Result` to construct `toField`](supported-transformations.md#wiring-the-constructors-parameter-to-a-provided-value)                              |
| `.withFieldConstPartial(_.matchingSome.toField, value)`                                             | the same as above (but a field is in `Option`)                                                                                                                              |
| `.withFieldConstPartial(_.everyItem.toField, value)`                                                | the same as above (but a field is in collection)                                                                                                                            |
| `.withFieldComputed(_.toField, from => ...)`                                                        | use the [provided function to construct `toField` from `From`](supported-transformations.md#wiring-the-constructors-parameter-to-the-computed-value)                        |
| `.withFieldComputed(_.matchingSome.toField, from => ...)`                                           | the same as above (but a field is in `Option`)                                                                                                                              |
| `.withFieldComputed(_.everyItem.toField, from => ...)`                                              | the same as above (but a field is in collection)                                                                                                                            |
| `.withFieldComputedFrom(_.fromField)(_.toField, fromField => ...)`                                  | use the [provided function to construct `toField` from `fromField`](supported-transformations.md#wiring-the-constructors-parameter-to-the-computed-value)                   |
| `.withFieldComputedFrom(_.matchingSome.fromField)(_.matchingSome.toField, fromField => ...)`        | the same as above (but fields are in `Option`)                                                                                                                              |
| `.withFieldComputedFrom(_.everyItem.fromField)(_.everyItem.toField, fromField => ...)`              | the same as above (but fields are in collection)                                                                                                                            |
| `.withFieldComputedPartial(_.toField, from => ...)`                                                 | use the [provided function to construct `toField` from `From`](supported-transformations.md#wiring-the-constructors-parameter-to-the-computed-value)                        |
| `.withFieldComputedPartial(_.matchingSome.toField, from => ...)`                                    | the same as above (but fields are in `Option`s)                                                                                                                             |
| `.withFieldComputedPartial(_.everyItem.toField, from => ...)`                                       | the same as above (but fields are in collections)                                                                                                                           |
| `.withFieldComputedPartialFrom(_.fromField)(_.toField, fromField => ...)`                           | use the [provided function to construct `toField` from `fromField`](supported-transformations.md#wiring-the-constructors-parameter-to-the-computed-value)                   |
| `.withFieldComputedPartialFrom(_.matchingSome.fromField)(_.matchingSome.toField, fromField => ...)` | the same as above (but fields are in `Option`s)                                                                                                                             |
| `.withFieldComputedPartialFrom(_.everyItem.fromField)(_.everyItem.toField, fromField => ...)`       | the same as above (but fields are in collections)                                                                                                                           |
| `.withSealedSubtypeHandled { (subtype: FromSubtype) => ... }`                                       | when pattern matching on `From`, use the [provided function to handle `FromSubtype`](supported-transformations.md#handling-a-specific-sealed-subtype-with-a-computed-value) |
| `.withEnumCaseHandled { (subtype: FromSubtype) => ... }`                                            | the same as above                                                                                                                                                           |
| `.withFieldComputedFrom(_.matching[FromSubtype]) { subtype => ... }`                                | the same as above                                                                                                                                                           |
| `.withSealedSubtypeHandledPartial { (subtype: FromSubtype) => ... }`                                | when pattern matching on `From`, use the [provided function to handle `FromSubtype`](supported-transformations.md#handling-a-specific-sealed-subtype-with-a-computed-value) |
| `.withEnumCaseHandledPartial { (subtype: FromSubtype) => ... }`                                     | the same as above                                                                                                                                                           |
| `.withFieldComputedPartialFrom(_.matching[FromSubtype]) { subtype => ... }`                         | the same as above                                                                                                                                                           |
| `.withFieldRenamed(_.fromField, _.toField)`                                                         | use the [`fromField` value to construct `toField`](supported-transformations.md#wiring-the-constructors-parameter-to-its-source-field)                                      |
| `.withFieldRenamed(_.matchingSome.fromField, _.matchingSome.toField)`                               | the same as above (but fields are in `Option`s)                                                                                                                             |
| `.withFieldRenamed(_.everyItem.fromField, _.everyItem.toField)`                                     | the same as above (but fields are in collections)                                                                                                                           |
| `.withSealedSubtypeRenamed[FromSubtype, ToSubtype]`                                                 | use the [`FromSubtype` value to construct `ToSubtype`](supported-transformations.md#handling-a-specific-sealed-subtype-by-a-specific-target-subtype)                        |
| `.withEnumCaseRenamed[FromSubtype, ToSubtype]`                                                      | the same as above                                                                                                                                                           |
| `.withFieldRenamed(_.matching[FromSubtype], _.matching[ToSubtype])`                                 | the same as above                                                                                                                                                           |
| `.withFieldUnused(_.fromField)`                                                                     | `fromField` should not be used, [`UnusedFieldPolicy` error](cookbook.md#checking-for-unused-source-fieldsunmatched-target-subtypes) should be suppressed                    |
| `.withSealedSubtypeUnmatched[ToSubtype]`                                                            | `ToSubtype` should not be matched, [`UnmatchedSubtypePolicy` error](cookbook.md#checking-for-unused-source-fieldsunmatched-target-subtypes) should be suppressed            |
| `.withEnumCaseUnmatched[ToSubtype]`                                                                 | the same as above should be suppressed                                                                                                                                      |

!!! example "Providing Transformer flags"

    ```scala
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.{Transformer, PartialTransformer}

    // Flags can be added per derivation
    source.into[Target].disableMacrosLogging.transform
    source.intoPartial[Target].disableMacrosLogging.transform
    Transformer.define[Source, Target].disableMacrosLogging.buildTransformer
    PartialTransformer.define[Source, Target].disableMacrosLogging.buildTransformer

    // Or for whole scope:
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.disableMacrosLogging

      source.transformInto[Target]
      source.transformIntoPartial[Target]
      Transformer.derive[Source, Target]
      PartialTransformer.derive[Source, Target]
    }
    ```

| Flag example                                                  | What it does                                                                                                                                                                              |
|---------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `.enableMethodAccessors`                                      | turn on [Reading from methods](supported-transformations.md#reading-from-methods)                                                                                                         |
| `.disableMethodAccessors`                                     | turn off [Reading from methods](supported-transformations.md#reading-from-methods) (default)                                                                                              |
| `.enableInheritedAccessors`                                   | turn on [Reading from inherited values/methods](supported-transformations.md#reading-from-inherited-valuesmethods)                                                                        |
| `.disableInheritedAccessors`                                  | turn off [Reading from inherited values/methods](supported-transformations.md#reading-from-inherited-valuesmethods) (default)                                                             |
| `.enableBeanGetters`                                          | turn on [Reading from Bean getters](supported-transformations.md#reading-from-bean-getters)                                                                                               |
| `.disableBeanGetters`                                         | turn off [Reading from Bean getters](supported-transformations.md#reading-from-bean-getters) (default)                                                                                    |
| `.enableBeanSetters`                                          | turn on [Writing to Bean setters](supported-transformations.md#writing-to-bean-setters)                                                                                                   |
| `.disableBeanSetters`                                         | turn off [Writing to Bean setters](supported-transformations.md#writing-to-bean-setters) (default)                                                                                        |
| `.enableBeanSettersIgnoreUnmatched`                           | turn on [Ignoring unmatched Bean setters](supported-transformations.md#ignoring-unmatched-bean-setters)                                                                                   |
| `.disableBeanSettersIgnoreUnmatched`                          | turn off [Ignoring unmatched Bean setters](supported-transformations.md#ignoring-unmatched-bean-setters) (default)                                                                        |
| `.enableNonUnitBeanSetters`                                   | turn on [Writing to non-`Unit` Bean setters](supported-transformations.md#writing-to-non-unit-bean-setters)                                                                               |
| `.disableNonUnitBeanSetters`                                  | turn off [Writing to non-`Unit` Bean setters](supported-transformations.md#writing-to-non-unit-bean-setters) (default)                                                                    |
| `.enableDefaultValues`                                        | turn on [Allowing fallback to the constructor's default values](supported-transformations.md#allowing-fallback-to-the-constructors-default-values)                                        |
| `.disableDefaultValues`                                       | turn off [Allowing fallback to the constructor's default values](supported-transformations.md#allowing-fallback-to-the-constructors-default-values) (default)                             |
| `.enableOptionDefaultsToNone`                                 | turn on [Allowing fallback to `None` as the constructor's argument](supported-transformations.md#allowing-fallback-to-none-as-the-constructors-argument)                                  |
| `.disableOptionDefaultsToNone`                                | turn off [Allowing fallback to `None` as the constructor's argument](supported-transformations.md#allowing-fallback-to-none-as-the-constructors-argument) (default)                       |
| `.enableNonAnyValWrappers`                                    | turn on [Transformation from/into a wrapper type](supported-transformations.md#frominto-a-wrapper-type)                                                                                   |
| `.disableNonAnyValWrappers`                                   | turn off [Transformation from/into a wrapper type](supported-transformations.md#frominto-a-wrapper-type) (default)                                                                        |
| `.enablePartialUnwrapsOption`                                 | turn on [Controlling automatic `Option` unwrapping](supported-transformations.md#controlling-automatic-option-unwrapping)  (default)                                                      |
| `.disablePartialUnwrapsOption`                                | turn off [Controlling automatic `Option` unwrapping](supported-transformations.md#controlling-automatic-option-unwrapping)                                                                |
| `.enableOptionFallbackMerge(SourceOrElseFallback)`            | turn on [merging `Option`s with `orElse`](supported-transformations.md#merging-option-with-option-into-option) taking source before fallback                                              |
| `.enableOptionFallbackMerge(FallbackOrElseSource)`            | turn on [merging `Option`s with `orElse`](supported-transformations.md#merging-option-with-option-into-option) taking fallback before source                                              |
| `.disableOptionFallbackMerge`                                 | turn off [merging `Option`s with `orElse`](supported-transformations.md#merging-option-with-option-into-option) (default)                                                                 |
| `.enableEitherFallbackMerge(SourceOrElseFallback)`            | turn on [merging `Either`s with `orElse`](supported-transformations.md#merging-either-with-either-into-either) taking source before fallback                                              |
| `.enableEitherFallbackMerge(FallbackOrElseSource)`            | turn on [merging `Either`s with `orElse`](supported-transformations.md#merging-either-with-either-into-either) taking fallback before source                                              |
| `.disableEitherFallbackMerge`                                 | turn off [merging `Either`s with `orElse`](supported-transformations.md#merging-either-with-either-into-either) (default)                                                                 |
| `.enableCollectionFallbackMerge(SourceAppendFallback)`        | turn on [merging collections with `++`](supported-transformations.md#merging-collection-with-collection-into-collection) taking source before fallback                                    |
| `.enableCollectionFallbackMerge(FallbackAppendSource)`        | turn on [merging collections with `++`](supported-transformations.md#merging-collection-with-collection-into-collection) taking fallback before source                                    |
| `.disableCollectionFallbackMerge`                             | turn off [merging `collections with `++`](supported-transformations.md#merging-collection-with-collection-into-collection) (default)                                                      |
| `.enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal)`       | turn on [Checking for unused fields](cookbook.md#checking-for-unused-source-fieldsunmatched-target-subtypes) to fail on unused                                                            |
| `.disableUnusedFieldPolicyCheck`                              | turn off [Checking for unused fields](cookbook.md#checking-for-unused-source-fieldsunmatched-target-subtypes) (default)                                                                   |
| `.enableUnmatchedSubtypePolicy(FailOnUnmatchedTargetSubtype)` | turn on [Checking for unmatched subtypes](cookbook.md#checking-for-unused-source-fieldsunmatched-target-subtypes) to fail on unmatched                                                    |
| `.disableUnmatchedSubtypePolicy`                              | turn off [Checking for unmatched subtypes](cookbook.md#checking-for-unused-source-fieldsunmatched-target-subtypes) (default)                                                              |
| `.enableImplicitConflictResolution(PreferTotalTransformer)`   | turn on [Resolving priority of implicit Total vs Partial Transformers](supported-transformations.md#resolving-priority-of-implicit-total-vs-partial-transformers) to Total Transformers   |
| `.enableImplicitConflictResolution(PreferPartialTransformer)` | turn on [Resolving priority of implicit Total vs Partial Transformers](supported-transformations.md#resolving-priority-of-implicit-total-vs-partial-transformers) to Partial Transformers |
| `.disableImplicitConflictResolution`                          | turn off [Resolving priority of implicit Total vs Partial Transformers](supported-transformations.md#resolving-priority-of-implicit-total-vs-partial-transformers) (default)              |
| `.enableMacrosLogging`                                        | turn on [Debugging macros](troubleshooting.md#debugging-macros)                                                                                                                           |
| `.disableMacrosLogging`                                       | turn off [Debugging macros](troubleshooting.md#debugging-macros) (default)                                                                                                                |

### Patchers' customization

All flags and overrides are described in more detail in the [Supported Patching page](supported-patching.md).

!!! note

    Examples below assume:

     *  patching `A` using `Patch`
     * `aField: AField`
     * `patchField: PatchField`

    for convention.

    While they show only path selectors like `.aField`/`.patchField`/`.matchingSome`/`.everyItem`, you can
    also use `.matchingLeft`/`.matchingRight`/`.everyMapKey`/`.everyMapValue`, combine them
    together, etc. These examples aren't an exhaustive list but just show what is possible.

    You can chain multiple overrides together.

| Override example                                                                              | What it does                                                                                                                      |
|-----------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------------------------------------------|
| `.withFieldConst(_.aField, value)`                                                            | use the [provided value to update `aField`](supported-patching.md#updating-field-with-a-provided-value)                           |
| `.withFieldConst(_.matchingSome.aField, value)`                                               | the same as above (but a field is in `Option`)                                                                                    |
| `.withFieldConst(_.everyItem.aField, value)`                                                  | the same as above (but a field is in collection)                                                                                  |
| `.withFieldComputed(_.aField, patch => ...)`                                                  | use the [provided function to update `toField` from `patch`](supported-patching.md#updating-field-with-a-computed-value)          |
| `.withFieldComputed(_.matchingSome.aField, patch => ...)`                                     | the same as above (but a field is in `Option`)                                                                                    |
| `.withFieldComputed(_.everyItem.aField, patch => ...)`                                        | the same as above (but a field is in collection)                                                                                  |
| `.withFieldComputedFrom(_.patchField)(_.aField, patchField => ...)`                           | use the [provided function to update `toField` from `patchField`](supported-patching.md#updating-field-with-a-computed-value)     |
| `.withFieldComputedFrom(_.matchingSome.patchField)(_.matchingSome.aField, patchField => ...)` | the same as above (but fields are in `Option`s)                                                                                   |
| `.withFieldComputedFrom(_.everyItem.patchField)(_.everyItem.aField, patchField => ...)`       | the same as above (but a field is in collection)                                                                                  |
| `.withFieldIgnored(_.patchField)`                                                             | `patchField` should not be used, [derivation should not complain](supported-patching.md#ignoring-fields-in-patches) that it isn't |
| `.withFieldIgnored(_.matchingSome.patchField)`                                                | the same as above (but fields are in `Option`s)                                                                                   |
| `.withFieldIgnored(_.everyItem.patchField)`                                                   | the same as above (but a field is in collection)                                                                                  |

!!! example "Providing Patcher flags"

    ```scala
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.Patcher

    // Flags can be added per derivation
    obj.using(patch).disableMacrosLogging.patch
    Patcher.define[A, Patch].disableMacrosLogging.buildPatcher

    // Or for whole scope:
    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = PatcherConfiguration.default.disableMacrosLogging

      obj.patchUsing(patch)
      Patcher.derive[A, Patch]
    }
    ```

| Flag example                    | What it does                                                                                                                                          |
|---------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| `.ignoreRedundantPatcherFields` | turn on [Ignoring fields in patches](supported-patching.md#ignoring-fields-in-patches)                                                                |
| `.failRedundantPatcherFields`   | turn off [Ignoring fields in patches](supported-patching.md#ignoring-fields-in-patches) (default)                                                     |
| `.ignoreNoneInPatch`            | turn on [Treating `None` as no-update instead of "set to `None`"](supported-patching.md#treating-none-as-no-update-instead-of-set-to-none)            |
| `.clearOnNoneInPatch`           | turn off [Treating `None` as no-update instead of "set to `None`"](supported-patching.md#treating-none-as-no-update-instead-of-set-to-none) (default) |
| `.ignoreLeftInPatch`            | turn on [Treating `Left` as no-update instead of "set to `Left`"](supported-patching.md#treating-left-as-no-update-instead-of-set-to-left)            |
| `.useLeftOnLeftInPatch`         | turn off [Treating `Left` as no-update instead of "set to `Left`"](supported-patching.md#treating-left-as-no-update-instead-of-set-to-left) (default) |
| `.appendCollectionInPatch`      | turn on [Appending to collection instead of replacing it](supported-patching.md#appending-to-collection-instead-of-replacing-it)                      |
| `.overrideCollectionInPatch`    | turn off [Appending to collection instead of replacing it](supported-patching.md#appending-to-collection-instead-of-replacing-it) (default)           |
| `.enableMacrosLogging`          | turn on [Debugging macros](troubleshooting.md#debugging-macros)                                                                                       |
| `.disableMacrosLogging`         | turn off [Debugging macros](troubleshooting.md#debugging-macros) (default)                                                                            |

## Integrations

Only some types would require writing some implicits to handle them. This might include: new type libraries,
custom collections (that don't implement Scala's collections interfaces), custom optional types, etc.

Integrations are described in more detail in [Integrations section](cookbook.md#integrations).

Before writing one, it's worth knowing that:

 * [Cats' types have an integration implemented](cookbook.md#cats-integration)
 * [Java's types have an integration implemented](cookbook.md#java-collections-integration)
 * [Protobufs have an integration implemented](cookbook.md#protocol-buffers-integration)

!!! tip

    All integrations for `Transformer`s work with `PartialTransformer`s and `Patcher`s as well!

### Total Transformers' integrations

??? example "Providing and using implicit Transformers"

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

??? example "Providing integrations, more flexible than hardcoded Transformer"

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

### Partial Transformations' integrations

??? example "Providing and using implicit PartialTransformers"

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

??? example "Providing integrations, more flexible than hardcoded Transformer"

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

??? example "Providing and using implicit Patcher"

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
