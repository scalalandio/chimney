# Cookbook

Examples of various use cases already handled by Chimney.

## Reusing the flags for several transformations/patchings

If we do not want to enable the same flag(s) in several places, we can define shared flag configuration as an implicit:

!!! example

    Scala 2

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    implicit val transformerCfg = TransformerConfiguration.default.enableMethodAccessors.enableMacrosLogging

    implicit val patcherCfg = PatcherConfiguration.default.ignoreNoneInPatch.enableMacrosLogging
    ```  

    Scala 3

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    transparent inline given TransformerConfiguration[?] =
      TransformerConfiguration.default.enableMethodAccessors.enableMacrosLogging

    transparent inline given PatcherConfiguration[?] =
      PatcherConfiguration.default.ignoreNoneInPatch.enableMacrosLogging
    ```  

!!! tip

    As we can see, on Scala 3 we can skip useless names, but we are required to provide the type. Since we want it to
    be inferred (for our convenience), we can use `transparent inline` to provide this time as a wildcard type but still
    let Scala figure it out.
    
!!! tip

    These configs will be shared by all derivations triggered in the scope that this `implicit`/`given` was defined.
    This includes automatic derivation as well, so summoning automatically derived Transformer would adhere to these
    flags.

!!! warning

    Since 0.8.0, Chimney assumes that you do NOT want to use the use implicit Transformer if you passed any `withField*`
    or `withSealedSubtype*` customization - using an implicit would not make it possible to do so. However, setting any flag
    with `enable*` or `disable*` would not prevent using implicit. So you could have situation like:
    
    ```scala
    implicit val foo2bar: Transformer[Foo, Bar] = ??? // stub for what is actually here
    foo.into[Bar].enableDefaultValues.transform // uses foo2bar ignoring flags
    ```
    
    Since 0.8.1, Chimney would ignore an implicit if any flag was explicitly used in `.into.transform`. Flags defined in
    an implicit `TransformerConfiguration` would be considerd new default settings in new derivations, but would not
    cause `.into.transform` to ignore an implicit if one is present. 

## Changing the flags for every derivation in the project

While `TransformerConfiguration` let us share configs (flags) between several derivations, we might also want to set up
some of them globally, for the whole project. Luckily, Scala 2.12, 2.13 and 3.3 give us `-Xmacro-settings` flag, which
is intended to pass configuration into the macros.

!!! example

    ```scala
    // in build.sbt:
    
    // log the derivation of every Transformer in project
    scalacOptions += "-Xmacro-settings:chimney.transformer.MacrosLogging=true"
    ```

!!! notice

    `-Xmacro-settings:...` is comma-separated, so if you want to pass multiple options, then either
    
      * provide this option more than once (`-Xmacro-settings:option-a -Xmacro-settings:option-b`)
      * provide this option one separating then with a coma (`-Xmacro-settings:option-a,option-b`)

As you can see `Transformer`'s flags have the prefix `chimney.transformer.`:

| Flag in DSL                                                   | Option for `-Xmacro-settings:...`                                         | Description                                                                                                                                                                               |
|---------------------------------------------------------------|---------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `.enableMethodAccessors`                                      | `chimney.transformer.MethodAccessors=true`                                | turn on [Reading from methods](supported-transformations.md#reading-from-methods)                                                                                                         |
| `.disableMethodAccessors`                                     | `chimney.transformer.MethodAccessors=false`                               | turn off [Reading from methods](supported-transformations.md#reading-from-methods) (default)                                                                                              |
| `.enableInheritedAccessors`                                   | `chimney.transformer.InheritedAccessors=true`                             | turn on [Reading from inherited values/methods](supported-transformations.md#reading-from-inherited-valuesmethods)                                                                        |
| `.disableInheritedAccessors`                                  | `chimney.transformer.InheritedAccessors=false`                            | turn off [Reading from inherited values/methods](supported-transformations.md#reading-from-inherited-valuesmethods) (default)                                                             |
| `.enableBeanGetters`                                          | `chimney.transformer.BeanGetters=true`                                    | turn on [Reading from Bean getters](supported-transformations.md#reading-from-bean-getters)                                                                                               |
| `.disableBeanGetters`                                         | `chimney.transformer.BeanGetters=false`                                   | turn off [Reading from Bean getters](supported-transformations.md#reading-from-bean-getters) (default)                                                                                    |
| `.enableBeanSetters`                                          | `chimney.transformer.BeanSetters=true`                                    | turn on [Writing to Bean setters](supported-transformations.md#writing-to-bean-setters)                                                                                                   |
| `.disableBeanSetters`                                         | `chimney.transformer.BeanSetters=false`                                   | turn off [Writing to Bean setters](supported-transformations.md#writing-to-bean-setters) (default)                                                                                        |
| `.enableBeanSettersIgnoreUnmatched`                           | `chimney.transformer.BeanSettersIgnoreUnmatched=true`                     | turn on [Ignoring unmatched Bean setters](supported-transformations.md#ignoring-unmatched-bean-setters)                                                                                   |
| `.disableBeanSettersIgnoreUnmatched`                          | `chimney.transformer.BeanSettersIgnoreUnmatched=false`                    | turn off [Ignoring unmatched Bean setters](supported-transformations.md#ignoring-unmatched-bean-setters) (default)                                                                        |
| `.enableNonUnitBeanSetters`                                   | `chimney.transformer.NonUnitBeanSetters=true`                             | turn on [Writing to non-`Unit` Bean setters](supported-transformations.md#writing-to-non-unit-bean-setters)                                                                               |
| `.disableNonUnitBeanSetters`                                  | `chimney.transformer.NonUnitBeanSetters=false`                            | turn off [Writing to non-`Unit` Bean setters](supported-transformations.md#writing-to-non-unit-bean-setters) (default)                                                                    |
| `.enableDefaultValues`                                        | `chimney.transformer.DefaultValues=true`                                  | turn on [Allowing fallback to the constructor's default values](supported-transformations.md#allowing-fallback-to-the-constructors-default-values)                                        |
| `.disableDefaultValues`                                       | `chimney.transformer.DefaultValues=false`                                 | turn off [Allowing fallback to the constructor's default values](supported-transformations.md#allowing-fallback-to-the-constructors-default-values) (default)                             |
| `.enableOptionDefaultsToNone`                                 | `chimney.transformer.OptionDefaultsToNone=true`                           | turn on [Allowing fallback to `None` as the constructor's argument](supported-transformations.md#allowing-fallback-to-none-as-the-constructors-argument)                                  |
| `.disableOptionDefaultsToNone`                                | `chimney.transformer.OptionDefaultsToNone=false`                          | turn off [Allowing fallback to `None` as the constructor's argument](supported-transformations.md#allowing-fallback-to-none-as-the-constructors-argument) (default)                       |
| `.enableNonAnyValWrappers`                                    | `chimney.transformer.NonAnyValWrappers=true`                              | turn on [Transformation from/into a wrapper type](supported-transformations.md#frominto-a-wrapper-type)                                                                                   |
| `.disableNonAnyValWrappers`                                   | `chimney.transformer.NonAnyValWrappers=false`                             | turn off [Transformation from/into a wrapper type](supported-transformations.md#frominto-a-wrapper-type) (default)                                                                        |
| `.enablePartialUnwrapsOption`                                 | `chimney.transformer.PartialUnwrapsOption=true`                           | turn on [Controlling automatic `Option` unwrapping](supported-transformations.md#controlling-automatic-option-unwrapping)  (default)                                                      |
| `.disablePartialUnwrapsOption`                                | `chimney.transformer.PartialUnwrapsOption=false`                          | turn off [Controlling automatic `Option` unwrapping](supported-transformations.md#controlling-automatic-option-unwrapping)                                                                |
| `.enableOptionFallbackMerge(SourceOrElseFallback)`            | `chimney.transformer.OptionFallbackMerge=SourceOrElseFallback`            | turn on [merging `Option`s with `orElse`](supported-transformations.md#merging-option-with-option-into-option) taking source before fallback                                              |
| `.enableOptionFallbackMerge(FallbackOrElseSource)`            | `chimney.transformer.OptionFallbackMerge=FallbackOrElseSource`            | turn on [merging `Option`s with `orElse`](supported-transformations.md#merging-option-with-option-into-option) taking fallback before source                                              |
| `.disableOptionFallbackMerge`                                 | `chimney.transformer.OptionFallbackMerge=none`                            | turn off [merging `Option`s with `orElse`](supported-transformations.md#merging-option-with-option-into-option) (default)                                                                 |
| `.enableEitherFallbackMerge(SourceOrElseFallback)`            | `chimney.transformer.EitherFallbackMerge=SourceOrElseFallback`            | turn on [merging `Either`s with `orElse`](supported-transformations.md#merging-either-with-either-into-either) taking source before fallback                                              |
| `.enableEitherFallbackMerge(FallbackOrElseSource)`            | `chimney.transformer.EitherFallbackMerge=FallbackOrElseSource`            | turn on [merging `Either`s with `orElse`](supported-transformations.md#merging-either-with-either-into-either) taking fallback before source                                              |
| `.disableEitherFallbackMerge`                                 | `chimney.transformer.EitherFallbackMerge=none`                            | turn off [merging `Either`s with `orElse`](supported-transformations.md#merging-either-with-either-into-either) (default)                                                                 |
| `.enableCollectionFallbackMerge(SourceAppendFallback)`        | `chimney.transformer.CollectionFallbackMerge=SourceAppendFallback`        | turn on [merging collections with `++`](supported-transformations.md#merging-collection-with-collection-into-collection) taking source before fallback                                    |
| `.enableCollectionFallbackMerge(FallbackAppendSource)`        | `chimney.transformer.CollectionFallbackMerge=FallbackAppendSource`        | turn on [merging collections with `++`](supported-transformations.md#merging-collection-with-collection-into-collection) taking fallback before source                                    |
| `.disableCollectionFallbackMerge`                             | `chimney.transformer.CollectionFallbackMerge=none`                        | turn off [merging `collections with `++`](supported-transformations.md#merging-collection-with-collection-into-collection) (default)                                                      |
| `.enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal)`       | `chimney.transformer.UnusedFieldPolicy=FailOnIgnoredSourceVal`            | turn on [Checking for unused fields](cookbook.md#checking-for-unused-source-fieldsunmatched-target-subtypes) to fail on unused                                                            |
| `.disableUnusedFieldPolicyCheck`                              | `chimney.transformer.UnusedFieldPolicy=none`                              | turn off [Checking for unused fields](cookbook.md#checking-for-unused-source-fieldsunmatched-target-subtypes) (default)                                                                   |
| `.enableUnmatchedSubtypePolicy(FailOnUnmatchedTargetSubtype)` | `chimney.transformer.UnmatchedSubtypePolicy=FailOnUnmatchedTargetSubtype` | turn on [Checking for unmatched subtypes](cookbook.md#checking-for-unused-source-fieldsunmatched-target-subtypes) to fail on unmatched                                                    |
| `.disableUnmatchedSubtypePolicy`                              | `chimney.transformer.UnmatchedSubtypePolicy=none`                         | turn off [Checking for unmatched subtypes](cookbook.md#checking-for-unused-source-fieldsunmatched-target-subtypes) (default)                                                              |
| `.enableImplicitConflictResolution(PreferTotalTransformer)`   | `chimney.transformer.ImplicitConflictResolution=PreferTotalTransformer`   | turn on [Resolving priority of implicit Total vs Partial Transformers](supported-transformations.md#resolving-priority-of-implicit-total-vs-partial-transformers) to Total Transformers   |
| `.enableImplicitConflictResolution(PreferPartialTransformer)` | `chimney.transformer.ImplicitConflictResolution=PreferPartialTransformer` | turn on [Resolving priority of implicit Total vs Partial Transformers](supported-transformations.md#resolving-priority-of-implicit-total-vs-partial-transformers) to Partial Transformers |
| `.disableImplicitConflictResolution`                          | `chimney.transformer.ImplicitConflictResolution=none`                     | turn off [Resolving priority of implicit Total vs Partial Transformers](supported-transformations.md#resolving-priority-of-implicit-total-vs-partial-transformers) (default)              |
| `.enableMacrosLogging`                                        | `chimney.transformer.MacrosLogging=true`                                  | turn on [Debugging macros](troubleshooting.md#debugging-macros)                                                                                                                           |
| `.disableMacrosLogging`                                       | `chimney.transformer.MacrosLogging=false`                                 | turn off [Debugging macros](troubleshooting.md#debugging-macros) (default)                                                                                                                |

`Patcher`'s flags have the prefix`chimney.patcher.`:

| Flag in DSL                     | Option for `-Xmacro-settings:...`                    | Description                                                                                                                                           |
|---------------------------------|------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------|
| `.ignoreRedundantPatcherFields` | `chimney.patcher.IgnoreRedundantPatcherFields=true`  | turn on [Ignoring fields in patches](supported-patching.md#ignoring-fields-in-patches)                                                                |
| `.failRedundantPatcherFields`   | `chimney.patcher.IgnoreRedundantPatcherFields=false` | turn off [Ignoring fields in patches](supported-patching.md#ignoring-fields-in-patches) (default)                                                     |
| `.ignoreNoneInPatch`            | `chimney.patcher.IgnoreNoneInPatch=true`             | turn on [Treating `None` as no-update instead of "set to `None`"](supported-patching.md#treating-none-as-no-update-instead-of-set-to-none)            |
| `.clearOnNoneInPatch`           | `chimney.patcher.IgnoreNoneInPatch=false`            | turn off [Treating `None` as no-update instead of "set to `None`"](supported-patching.md#treating-none-as-no-update-instead-of-set-to-none) (default) |
| `.ignoreLeftInPatch`            | `chimney.patcher.IgnoreLeftInPatch=true`             | turn on [Treating `Left` as no-update instead of "set to `Left`"](supported-patching.md#treating-left-as-no-update-instead-of-set-to-left)            |
| `.useLeftOnLeftInPatch`         | `chimney.patcher.IgnoreLeftInPatch=false`            | turn off [Treating `Left` as no-update instead of "set to `Left`"](supported-patching.md#treating-left-as-no-update-instead-of-set-to-left) (default) |
| `.appendCollectionInPatch`      | `chimney.patcher.AppendCollectionInPatch=true`       | turn on [Appending to collection instead of replacing it](supported-patching.md#appending-to-collection-instead-of-replacing-it)                      |
| `.overrideCollectionInPatch`    | `chimney.patcher.AppendCollectionInPatch=false`      | turn off [Appending to collection instead of replacing it](supported-patching.md#appending-to-collection-instead-of-replacing-it) (default)           |
| `.enableMacrosLogging`          | `chimney.patcher.MacrosLogging=true`                 | turn on [Debugging macros](troubleshooting.md#debugging-macros)                                                                                       |
| `.disableMacrosLogging`         | `chimney.patcher.MacrosLogging=false`                | turn off [Debugging macros](troubleshooting.md#debugging-macros) (default)                                                                            |

### Suppressing warnings in macros

There are additional global options only available through `-Xmacro-settings`. They can be used to add annotations
`@java.lang.SuppressWarnings` and `@scala.annotation.nowarn`, which is useful in suppressing warnings from plug-ins
like [WartRemover](https://www.wartremover.org/) or [Scapegoat](https://github.com/scapegoat-scala/scapegoat).

| Option for `-Xmacro-settings:...`            | Description                                                                   |
|----------------------------------------------|-------------------------------------------------------------------------------|
| `chimney.SuppressWarnings=warning1;warning2` | annotates the generated code with `@SuppressWarnings("warning1", "warning2")` |
| `chimney.SuppressWarnings=none`              | does not annotate the generated code with `@SuppressWarnings`                 |
| `chimney.nowarn=msg`                         | annotates the generated code with `@nowarn("msg")`                            |
| `chimney.nowarn=true`                        | annotates the generated code with `@nowarn`                                   |
| `chimney.nowarn=none`                        | does not annotate the generated code with `@nowarn`                           |

By default, code is annotated with`@SuppressWarnings("org.wartremover.warts.All", "all")` and without `@nowarn`. 

## Constraining the flags to a specific field/subtype

Flags set up immediately on a `Transformer` or a `TransformerConfiguration` will affect every field (if it configures
how to build a class) or every subtype (if it configures how to pattern-match a `sealed`/`enum`).

But we might want to e.g. allow using default values only in a specific field. To achieve that we can use
`.withSourceFlag`/`.withTargetFlag` methods:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}    
    import io.scalaland.chimney.dsl._
    
    case class User(id: Int, name: String, age: Option[Int])
    case class User2ID(id: Int, name: String, age: Option[Int], extraID: Int = 0)
    
    pprint.pprintln(
      User(1, "Adam", None)
        .into[User2ID]
        .withTargetFlag(_.extraID).enableDefaultValues
        .transform
    )
    // expected output:
    // User2ID(id = 1, name = "Adam", age = None, extraID = 0)
    ```

Whether a flag is a part of `.withSourceFlag` or `.withTargetFlag` depends on what it does:

 * when wiring input values to the constructor/setters, there are flags controlling which values can be used as inputs
   (e.g. inherited values, `def` methods, getters) and whether setters are allowed or not (if there are any). For these
   flags we are configuring the behavior for a particular constructor argument/setter, so the selector path in on
   the target side (`.withTargetFlag(pathToTarget)`)
 * when pattern matching on a `sealed` hierarchy/`enum` we are configuring how the subtype will be handled in a pattern
   match so the selector path is on the source side (`.withSourceFlag(pathFromSource)`)

Additionally, we need to be aware that some flags cannot act immediately,  on the path we defined it on, but on every
subtype/field of the path it is defined:

 * `.enableCustomFieldNameComparison` - since the comparator is (also) used to determine if a field has a flag defined
   for it, we cannot configure a comparator for a single field - the target path of the comparator will decide how 
   _all_ fields under this target path will be compared  
 * `.enableCustomSubtypeNameComparison` - since the comparator is (also) used to determine if a subtype has a flag
   defined for it, we cannot configure a comparator for a single subtype - the source path of the comparator will decide
   how _all_ fields under this target path will be compared

Some flags can only be set globally:

 * `.enableMacrosLogging` cannot be done for a single field/subtype

Similarly `Patcher` has `.withPatchedValueFlag(pathFromPatchedValue)`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}    
    import io.scalaland.chimney.dsl._
    
    case class User(id: Int, name: String, age: Option[Int])
    case class UserPatch(name: String, age: Option[Int], extraID: Int = 0)
    case class Nested[A](value: A)

    pprint.pprintln(
      Nested(User(1, "Adam", None))
        .using(Nested(UserPatch("Jogn", Some(10), 10)))
        .withPatchedValueFlag(_.value).ignoreRedundantPatcherFields
        .patch
    )
    // expected output:
    // Nested(value = User(id = 1, name = "Jogn", age = Some(value = 10)))
    ```

## Checking for unused source fields/unmatched target subtypes

While most of the time Chimney is picked for generating mapping between 2 data types wit as little hassle as possible,
some people use type mapping tools to express mapping as a declarative description of the transformation. As a Part of
that requirement is making it explicit, that some field in the source value was dropped, or that matching between 2
`sealed` hierarchies didn't use one target subtype.

These can be enabled with `UnusedFieldPolicy`:

!!! example "Field has to be explicitly ignored to compile"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}    
    import io.scalaland.chimney.dsl._
    
    case class User1(id: Int, name: String, age: Option[Int])
    case class User2(id: Int, name: String)
    
    pprint.pprintln(
      User1(1, "Adam", None)
        .into[User2]
        .withFieldUnused(_.age)
        .enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal)
        .transform
    )
    // expected output:
    // User2(id = 1, name = "Adam")

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal)

      pprint.pprintln(
        User1(1, "Adam", None)
          .into[User2]
          .withFieldUnused(_.age)
          .transform
      )
      // expected output:
      // User2(id = 1, name = "Adam")
    }
    ```

!!! example "Silent drop of a field causes failure"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}    
    import io.scalaland.chimney.dsl._
    
    case class User1(id: Int, name: String, age: Option[Int])
    case class User2(id: Int, name: String)
    
    pprint.pprintln(
      User1(1, "Adam", None)
        .into[User2]
        .enableUnusedFieldPolicyCheck(FailOnIgnoredSourceVal)
        .transform
    )
    // expected error:
    // Chimney can't derive transformation from User1 to User2
    // 
    // User2
    //   FailOnIgnoredSourceVal policy check failed at _, offenders: age!
    // 
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

and `UnmatchedSubtypePolicy`:

!!! example "Subptype has to be explicitly ignored to compile"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}    
    import io.scalaland.chimney.dsl._
    
    sealed trait RGB extends Product with Serializable
    object RGB {
      case object Red extends RGB
      case object Green extends RGB
      case object Blue extends RGB
    }

    sealed trait RGBA extends Product with Serializable
    object RGBA {
      case object Red extends RGBA
      case object Green extends RGBA
      case object Blue extends RGBA
      case object Alpha extends RGBA
    }
    
    pprint.pprintln(
      (RGB.Red: RGB)
        .into[RGBA]
        .withSealedSubtypeUnmatched(_.matching[RGBA.Alpha.type])
        .enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype)
        .transform
    )
    // expected output:
    // Red

    pprint.pprintln(
      (RGB.Green: RGB)
        .into[RGBA]
        .withEnumCaseUnmatched(_.matching[RGBA.Alpha.type])
        .enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype)
        .transform
    )
    // expected output:
    // Green

    locally {
      // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
      implicit val cfg = TransformerConfiguration.default.enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype)

      pprint.pprintln(
      (RGB.Blue: RGB)
        .into[RGBA]
        .withSealedSubtypeUnmatched(_.matching[RGBA.Alpha.type])
        .transform
      )
      // expected output:
      // Blue

      pprint.pprintln(
      (RGB.Red: RGB)
        .into[RGBA]
        .withEnumCaseUnmatched(_.matching[RGBA.Alpha.type])
        .transform
      )
      // expected output:
      // Red
    }
    ```

!!! example "Silent drop of a subtype causes failure"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}    
    import io.scalaland.chimney.dsl._
    
    sealed trait RGB extends Product with Serializable
    object RGB {
      case object Red extends RGB
      case object Green extends RGB
      case object Blue extends RGB
    }

    sealed trait RGBA extends Product with Serializable
    object RGBA {
      case object Red extends RGBA
      case object Green extends RGBA
      case object Blue extends RGBA
      case object Alpha extends RGBA
    }
    
    pprint.pprintln(
      (RGB.Red: RGB)
        .into[RGBA]
        .enableUnmatchedSubtypePolicyCheck(FailOnUnmatchedTargetSubtype)
        .transform
    )
    // expected error:
    // Chimney can't derive transformation from RGB to RGBA
    // 
    // RGBA
    //   FailOnUnmatchedTargetSubtype policy check failed at _, offenders: RGBA.Alpha!
    // 
    // Consult https://chimney.readthedocs.io for usage examples.
    ```

## Avoiding nested Transformers

In previous version of Chimney there way many cases when users were forced to define inner `Transformer` to customize
transformation of some nested value, or used `.withFieldComputed` running `.into....transform`. Newest versions
eliminated this requirement, and users need to define an implicit `Transformer`/`PartialTransformer` only if they
actually want to reuse some of them.

### Enabling flag only for one nested value

Let's say we want to enable method accessors in a nested case class. How most people would do this (due to restrictions of
older Chimney's versions) would be:

!!! example "Intermediate Transformer"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    class Foo (a: Int) {
      val value: String = a.toString
    }
    case class OuterFoo(inner: Foo)

    case class Bar(value: String)
    case class OuterBar(inner: Bar)

    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._

    implicit val innerTransformer: Transformer[Foo, Bar] =
      Transformer.define[Foo, Bar].enableMethodAccessors.buildTransformer

    pprint.pprintln(
      OuterFoo(new Foo(20)).transformInto[OuterBar]
    )
    // expected output:
    // OuterBar(inner = Bar(value = "20"))
    ```

or

!!! example "Nested .into.transform"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    class Foo (a: Int) {
      val value: String = a.toString
    }
    case class OuterFoo(inner: Foo)

    case class Bar(value: String)
    case class OuterBar(inner: Bar)

    import io.scalaland.chimney.dsl._

    pprint.pprintln(
      OuterFoo(new Foo(20)).into[OuterBar]
        .withFieldComputed(_.inner, foo => foo.inner.into[Bar].enableMethodAccessors.transform)
        .transform
    )
    // expected output:
    // OuterBar(inner = Bar(value = "20"))
    ```

Since Chimney 1.6.0 we are able to [scope the flag to a particular field](#constraining-the-flags-to-a-specific-fieldsubtype):

!!! example "Nested .into.transform"

    ```scala
    //> using dep io.scalaland::chimney::1.6.0
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    class Foo (a: Int) {
      val value: String = a.toString
    }
    case class OuterFoo(inner: Foo)

    case class Bar(value: String)
    case class OuterBar(inner: Bar)

    import io.scalaland.chimney.dsl._

    pprint.pprintln(
      OuterFoo(new Foo(20)).into[OuterBar]
        .withTargetFlag(_.inner).enableMethodAccessors
        .transform
    )
    // expected output:
    // OuterBar(inner = Bar(value = "20"))
    ```

If the particular flag we want to use in limited scope is `.enableDefaultValues`, we might also consider
[`.enableDefaultValueOfType[A]`](supported-transformations.md#allowing-fallback-to-the-constructors-default-values)
available since Chimney 1.2.0 (but scoped flag would work as well!).

!!! example "Enabling default values only for 1 type"

    ```scala
    //> using dep io.scalaland::chimney::1.2.0
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    class Foo
    case class OuterFoo(inner: Foo)

    case class Bar(value: String = "default")
    case class OuterBar(inner: Bar)

    import io.scalaland.chimney.dsl._

    pprint.pprintln(
      OuterFoo(new Foo()).into[OuterBar]
        .enableDefaultValueOfType[String]
        .transform
    )
    // expected output:
    // OuterBar(inner = Bar(value = "default"))
    ```

### Computing value from field at a different level of nesting

`.withFieldComputed` (or a locally defined `Transformer`) was used to work around a few limitation.

One of them was a requirement of only renaming fields at the same level of nesting.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class Foo(a: String)
    case class OuterFoo(inner: Foo)

    case class Bar(b: String)
    case class OuterBar(inner: Bar)

    import io.scalaland.chimney.dsl._

    pprint.pprintln(
      OuterFoo(Foo("value")).into[OuterBar]
        .withFieldComputed(_.inner, outer => outer.inner.into[Bar].withFieldRenamed(_.a, _.b).transform)
        .transform
    )
    // expected output:
    // OuterBar(inner = Bar(b = "value"))
    ```

This limitation was lifted since Chimney 1.0.0, and one can rename fields in nested `case class`es using
only `.withFieldRenamed`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::1.0.0
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class Foo(a: String)
    case class OuterFoo(inner: Foo)

    case class Bar(b: String)
    case class OuterBar(inner: Bar)

    import io.scalaland.chimney.dsl._

    pprint.pprintln(
      OuterFoo(Foo("value")).into[OuterBar]
        .withFieldRenamed(_.inner.a, _.inner.b)
        .transform
    )
    // expected output:
    // OuterBar(inner = Bar(b = "value"))
    ```

Another issue solved by nesting a transformation inside the `.withFieldComputed` (or creating intermediate
`Transformer`) was providing some function to compute the field in the target type, which should be wired
to some particular field in source value.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class Foo(a: Int)
    case class OuterFoo(inner: Foo, inner2: Foo)

    case class Bar(b: String)
    case class OuterBar(inner: Bar)

    def helper(input: Int): String = (input * 2).toString

    import io.scalaland.chimney.dsl._

    pprint.pprintln(
      OuterFoo(Foo(0), Foo(10)).into[OuterBar]
        .withFieldComputed(
          _.inner,
          outer => outer.inner.into[Bar].withFieldConst(_.b, helper(outer.inner2.a)).transform
        )
        .transform
    )
    // expected output:
    // OuterBar(inner = Bar(b = "20"))
    ```

We can wire input for such helpers using specialized [`.withFieldComputedFrom`](supported-transformations.md#wiring-the-constructors-parameter-to-the-computed-value)
available since Chimney 1.6.0:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::1.6.0
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class Foo(a: Int)
    case class OuterFoo(inner: Foo, inner2: Foo)

    case class Bar(b: String)
    case class OuterBar(inner: Bar)

    def helper(input: Int): String = (input * 2).toString

    import io.scalaland.chimney.dsl._

    pprint.pprintln(
      OuterFoo(Foo(0), Foo(10)).into[OuterBar]
        .withFieldComputedFrom(_.inner2.a)(_.inner.b, helper)
        .transform
    )
    // expected output:
    // OuterBar(inner = Bar(b = "20"))
    ```

### Customizing transformation within collection/map/Option/Either

Many users are not aware that Chimney can transform one Scala collection into another. You can still find examples like this:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class Foo(a: Int)
    case class Bar(a: Int)

    import io.scalaland.chimney.dsl._

    pprint.pprintln(
      List(Foo(10)).map { foo =>
        foo.transformInto[Bar]
      }
    )
    // expected output:
    // List(Bar(a = 10))
    ```

even though transforming all values of a collection (and even the type of a collection!) was supported since Chimney 0.2.0:

!!! example

    ```scala
    //> using scala {{ scala.2_12 }}
    //> using dep io.scalaland::chimney::0.2.0
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class Foo(a: Int)
    case class Bar(a: Int)

    import io.scalaland.chimney.dsl._

    pprint.pprintln(
      List(Foo(10)).transformInto[Vector[Bar]]
    )
    // expected output:
    // Vector(Bar(10))
    ```

However, every time one needed to customize how a value inside a collection is transformed, one was falling back to `.map`s
or intermediate transformers:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class Foo(a: Int)
    case class OuterFoo(values: List[Foo])

    case class Bar(a: Int, b: String)
    case class OuterBar(values: Vector[Bar])

    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._

    implicit val foo2bar: Transformer[Foo, Bar] =
      Transformer.define[Foo, Bar].withFieldConst(_.b, "value").buildTransformer

    pprint.pprintln(
      OuterFoo(List(Foo(10))).transformInto[OuterBar]
    )
    // expected output:
    // OuterBar(values = Vector(Bar(a = 10, b = "value")))
    ```

This is no longer needed, since Chimney 1.0.0 added an ability to customize collections, maps, `Option`s and `Either`s
with the DSL:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::1.0.0
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class Foo(a: Int)
    case class OuterFoo(values: List[Foo])

    case class Bar(a: Int, b: String)
    case class OuterBar(values: Vector[Bar])

    import io.scalaland.chimney.dsl._

    pprint.pprintln(
      OuterFoo(List(Foo(10))).into[OuterBar]
        .withFieldConst(_.values.everyItem.b, "value")
        .transform
    )
    // expected output:
    // OuterBar(values = Vector(Bar(a = 10, b = "value")))
    ```

Path selectors and examples for [collections, maps and arrays](supported-transformations.md#between-scalas-collectionsarrays),
[`Option`s](supported-transformations.md#frominto-an-option) and [`Either`s](supported-transformations.md#between-eithers)
are described in each type's section.

## Automatic, Semiautomatic and Inlined derivation

!!! note "Chimney is not like other libs" 

    If you are used to _automatic_ vs _semi-atuomatic_ derivation conventions from other libraries, like Circe, and you
    had bad experience (long compilation times, poor performance) with the automatic derivation, please note that
    Chimney derivation DOES NOT work the same way, so your experiences are unlikely to carry over to Chimney.
    
    Please, read the section below, as it will explain why replacing `import io.scalaland.chimney.dsl._` with
    `Transformer.derive` + `import io.scalaland.chimney.syntax._` + `import io.scalaland.chimney.auto._` might actually
    *degrade* the performance, instead of improving it.
    
    In depth explanation why automatic derivation is slow (when it's slow!) and how Chimney avoided such slowdown can be
    found in [*Slow-Auto, Inconvenient-Semi: escaping false dichotomy with sanely-automatic derivation*](https://mateuszkubuszok.github.io/SlowAutoInconvenientSemi/)
    presentation recorded in [Art of Scala](https://www.youtube.com/watch?v=scWvlO_fb78) and [Scala.io](https://www.youtube.com/watch?v=h9NdXLTZkGk),
    and expanded during [Scala Space podcast](https://www.youtube.com/watch?v=FUL4Ou1SDx4).  

When you use the standard way of working with Chimney, but `import io.scalaland.chimney.dsl._`
you might notice that it is a very convenient approach, making a lot of things easy:

  - when you want to trivially convert `val from: From` into `To` you can do
    it with `from.transformInto[To]`
  - the code above would be able to map case classes recursively
  - if you wanted to provide some transformation to use either directly in this
    `.transformInto` or in some nesting, you can do it just by using implicits
  - if you wanted to generate this implicit you could use `Transformer.derive`
  - if you needed to customize the derivation you could us
    `Transformer.define.customisationMethod.buildTransformer` or
    `from.into[To].customisationMethod.transform`

However, sometimes you may want to restrict this behavior. It might be too easy to:

  - derive the same transformation again and again
  - define some customized `Transformer`, not import it by accident and still
    end up with the compiling code since Chimney could derive a new one on the spot

### Automatic vs semiautomatic

In other libraries this issue is addressed by providing 2 flavors of derivation:

  - *automatic derivation*: usually requires some `import library.auto._`, allows you
    to get a derived instance just by summoning it e.g. with `implicitly[TypeClass[A]]`
    or calling any other method that would take it as an `implicit` parameter.
  
    Usually, it is convenient to use, but has a downside of re-deriving the same instance
    each time you need it. Additionally, you cannot write
  
    !!! example
  
        ```scala
        implicit val typeclass: TypeClass[A] = implicitly[TypeClass[A]]
        ```
  
    since that generates circular dependency on a value initialization. This makes it hard
    to cache this instance in e.g. companion object. In some libraries, it also makes it hard
    to use automatic derivation to work with recursive data structures.

  - *semi-automatic derivation*: requires you to explicitly call some method that will provide
    a derived instance. It has the downside that for each instance that you would like to summon
    you need to manually derive and assign to an `implicit val` or `def`

    !!! example

        ```scala
        implicit val typeclass: TypeClass[A] = deriveTypeClass[A]
        ```

    However, it gives you certainty that each time you need an instance of a type class
    it will be the one you manually created. It reduces compile time, and makes it easier
    to limit the places where error can happen (if you reuse the same instance everywhere
    and there is a bug in an instance, there is only one place to look for it).

The last property is a reason many projects encourage the usage of semiautomatic derivation
and many libraries provide automatic derivation as a quick and dirty way of doing things
requiring an opt-in.

Chimney's defaults for (good) historical reasons mix these 2 modes (and one more, which
will describe in a moment), but (_due to popular demand_) it also allows you to selectively
use these imports

!!! example

    ```scala
    import io.scalaland.chimney.auto._
    import io.scalaland.chimney.inlined._
    import io.scalaland.chimney.syntax._
    ```

instead of `io.scalaland.chimney.dsl` to achieve a similar behavior:

  - if you `import io.scalaland.chimney.syntax._` it will expose only extension
    methods working with type classes (`Transformer`, `PartialTransformer` and `Patcher`),
    but with no derivation

  - if you `import io.scalaland.chimney.auto._` it will only provide implicit instances
    generated through derivation.

    Semiautomatic derivation was available for a long time using methods:

    !!! example

        ```scala
        // Defaults only:
        Transformer.derive[From, To]
        PartialTransformer.derive[From, To]
        Patcher.derive[A, Patch]
        // Allows customization:
        Transformer.define[From, To].buildTransformer
        PartialTransformer.define[From, To].buildTransformer
        Patcher.define[A, Patch].buildPatcher
        ```

  - finally, there is `import io.scalaland.chimney.inlined._`. It provides extension methods:

    !!! example

        ```scala
        from.into[To].transform
        from.intoPartial[To].transform
        from.using[To].patch
        ```

    At the first glance, all they do is generate a customized type class before calling it, but
    what actually happens is that it generates an inlined expression, with no type class
    instantiation - if the user provided a type class for top-level or nested transformation it
    will be used, but wherever Chimney has to generate code ad hoc, it will generate inlined
    code. For that reason, this could be considered a third mode, one where generated code
    is non-reusable, but optimized to avoid any type class allocation and deferring
    `partial.Result` wrapping (in case of `PartialTransformer` s) as long as possible.

### Performance concerns

When Chimney derives an expression, whether that is an expression directly inlined at a call site
or as the body of the `transform`/`patch` method inside a type class instance, it attempts
to generate a fast code.

It contains special cases for `Option` s, `Either` s, it attempts to avoid boxing with
`partial.Result` and creating type classes if it can help it.

You can use [`.enableMacrosLogging`](troubleshooting.md#debugging-macros) to see the code generated by

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    case class Foo(baz: Foo.Baz)
    object Foo {
      case class Baz(a: String)
    }
    case class Bar(baz: Bar.Baz)
    object Bar {
      case class Baz(a: String)
    }

    Foo(Foo.Baz("string")).into[Bar].enableMacrosLogging.transform
    ```

The generated code (in the absence of implicits) should be

!!! example

    ```scala
    val foo = Foo(Foo.Baz("string"))
    new Bar(new Bar.Baz(foo.baz.a))
    ```

Similarly, when deriving a type class it would be

!!! example

    ```scala
    new Transformer[Foo, Bar] {
      def transform(foo: Foo): Bar =
        new Bar(new Bar.Baz(foo.baz.a))
    }
    ```

However, Chimney is only able to do it when given free rein. It checks
if the user provided an implicit, and if they did, it should be used instead.

In case of the automatic derivation, it means that every single branching
in the code - derivation for a field of a case class, or a subtype of a
sealed hierarchy - will trigger a macro, which may or may not succeed
and if it succeeds it will introduce an allocation.

When using `import io.scalaland.chimney.dsl._` this is countered by the usage of
a `Transformer.AutoDerived` as a supertype of `Transformer` - automatic
derivation upcast `Transformer` and recursive construction of an expression requires
a normal `Transformer` so automatic derivation is NOT triggered. Either the user provided
an implicit or there is none.

!!! note

    What it means for you is that Chimney will try to minimize the amount of macro expansions
    and achieve as much as possible withing the same expansion. `implicit Transformer`s and
    `PartialTransformer`s are only needed to *override* the default behavior, and they are *not*
    needed for the handling of every intermediate value.

However, with `import io.scalaland.chimney.auto._` the same semantics as in other
libraries is used: implicit def returns `Transformer`, so if derivation with defaults
is possible it will always be triggered.

The matter is even more complex with `PartialTransformer` s - they look for both implicit
`Transformer` s as well as implicit `PartialTransformer` s (users can provide either or both).
With the automatic derivation both versions could always be available, so users need to always
provide `implicitConflictResolution` flag.

!!! note

    In other words, replicating the setup where you do:
    
    ```scala
    implicit val transformer: Transformer[From, To] = locally {
      import io.scalaland.chimney.auto._
      Transformer.derive[From, To]
    }
    ```
    
    (which might be popular in other libaries when semi-automatic derivation is preferred) will introduce additional,
    unnecessary macro expansions, which could increase the compilation time and degrade the performance.
    
    This would not happen if you do:
    
    ```scala
    implicit val transformer: Transformer[From, To] = Transformer.derive[From, To]
    ```
    
    instead.

For the reasons above the recommendations are as follows:

  - if you care about performance, use either inlined derivation (`.into.transform`, for a one-time-usage) or
    semi-automatic derivation with recursion handled in the macro(`.derive`/`.define.build*` + `syntax._`, without
    importing `auto._`)
  - only use `import auto._` when you want predictable behavior similar to other libraries
    (predictably bad)
  - use unit tests to ensure, that your code does what it should do
  - use benchmarks to ensure it is reasonably fast
  - and keep on using `import dsl._` until you have some good proof that (recursive) semi-automatic derivation is needed 

## Bidirectional transformations

In some cases you might want to derive 2 transformations: from some type into another type and back. Most of the time,
such case appears not when you are using transformation on the spot, but when you need to derived in a semiautomatic
way.

!!! example "Isomorphic transformation"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.Transformer
    
    case class Foo(a: Int, b: String)
    case class Bar(b: String, a: Int)
    
    object Bar {
      implicit val fromFoo: Transformer[Foo, Bar] = Transformer.derive
      implicit val intoFoo: Transformer[Bar, Foo] = Transformer.derive
    }
    ```

!!! example "Domain model encoding/decoding"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.{PartialTransformer, Transformer}
    
    case class Domain(a: Int, b: String)
    case class Dto(b: Option[String], a: Option[Int])
    
    object Dto {
      implicit val fromDomain: Transformer[Domain, Dto] = Transformer.derive
      implicit val intoDomain: PartialTransformer[Dto, Domain] = PartialTransformer.derive
    }
    ```

To make things less annoying, in such cases you can use `Iso` (for bidirectional conversion that always succeeds)
or `Codec` (for bidirectional conversion which always succeeds in one way, but might need validation in another):

!!! example "Isomorphic transformation"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.Iso
    
    case class Foo(a: Int, b: String)
    case class Bar(b: String, a: Int)
    
    object Bar {
      implicit val iso: Iso[Foo, Bar] = Iso.derive
      // Provides:
      // - iso.first: Transformer[Foo, Bar]
      // - iso.second: Transformer[Bar, Foo]
      // both automatically unpacked when using the DSL.
    }
    ```

!!! example "Domain model encoding/decoding"

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.Codec
    
    case class Domain(a: Int, b: String)
    case class Dto(b: Option[String], a: Option[Int])
    
    object Dto {
      implicit val codec: Codec[Domain, Dto] = Codec.derive
      // Provides:
      // - codec.encode: Transformer[Foo, Bar]
      // - codec.decode: PartialTransformer[Bar, Foo]
      // both automatically unpacked when using the DSL.
    }
    ```

Both `Iso` and `Codec` are only available through semiautomatic derivation. Currently, they only provide
`withFieldRenamed` value override and flags overrides. 

## Java collections' integration

If you need support for:

  - `java.util.Optional` and convert to/from it as if it was `scala.Option`
  - `java.util.Collection`/`java.lang.Iterable`/`java.util.Enumerable` and convert to/from it as if it was
    `scala.collection.IterableOnce` with a dedicated `Factory` (or `CanBuildFrom`)
  - `java.util.Map`/`java.util.Dictionary`/`java.util.Properties` and convert to/from `scala.collection.Map`
  - `java.util.stream`s and convert them to/from all sorts of Scala collections
  - `java.lang.Boolean`/`java.lang.Byte`/`java.lang.Char`/`java.lang.Int`/`java.lang.Long`/`java.lang.Long`/
    `java.lang.Short`/`java.lang.Float`/`java.lang.Double` and convert to/from its `scala` counterpart

then you can use one simple import to enable it:

!!! example

    ```scala
    //> using dep io.scalaland::chimney-java-collections::{{ chimney_version() }}
    import io.scalaland.chimney.javacollections._
    ```

!!! warning

    There is an important performance difference between Chimney conversion and `scala.jdk.converions`.
    
    While `asJava` and `asScala` attempt to be O(1) operations, by creating a cheap wrapper around the original
    collection, Chimney creates a full copy. It is the only way to
    
      - target various specific implementations of the target type
      - guarantee that you don't merely wrap a mutable type which could be mutated right after you wrap it 

## Cats integration

Cats integration module contains the following utilities:

  - conversions between `partial.Result`s and `Validated` (and `ValidatedNel`, `ValidatedNec`) data type allowing 
    e.g. to convert `PartialTransformer`'s result to `Validated`
  - instances for Chimney types (many combined into single implicit to prevent conflicts):
     - for `Transformer` type class:
        - `ArrowChoice[Transformer] & CommutativeArrow[Transformer]` (implementing also `Arrow`, `Choice`, `Category`,
          `Compose`, `Strong`, `Profunctor`)
        - `[Source] => Monad[Transformer[Source, *]] & CoflatMap[Transformer[Source, *]]`
          (implementing also `Monad`, `Applicative`, `Functor`)
        - `[Target] => Contravariant[Transformer[*, Target]]` (implementing also `Invariant`)
     - for `PartialTransformer` type class:
        - `ArrowChoice[PartialTransformer] & CommutativeArrow[PartialTransformer]` (implementing also `Arrow`, `Choice`,
          `Category`,`Compose`, `Strong`, `Profunctor`)
        - `[Source] => MonadError[PartialTransformer[Source, *], partial.Result.Errors] & CoflatMap[PartialTransformer[Source, *]] & Alternative[PartialTransformer[Source, *]]`
          (implementing also `Monad`, `Applicative`, `Functor`, `ApplicativeError`, `NonEmptyAlternative`, `MonoidK`,
          `SemigroupK`)
        - `[Source] => Parallel[PartialTransformer[Source, *]]` (implementing also `NonEmptyParallel`)
        - `[Target] => Contravariant[Transformer[*, Target]]` (implementing also `Invariant`)
     - for `partial.Result` data type:
        - `MonadError[partial.Result, partial.Result.Errors] & CoflatMap[partial.Result] & Traverse[partial.Result] $ Alternative[partial.Result]`
          (implementing also `Monad`, `Applicative`, `Functor`, `ApplicativeError`, `UnorderedTraverse`, `Foldable`,
          `UnorderedFoldable`, `Invariant`, `Semigriupal`, `NonEmptyAlternative`, `SemigroupK`, `MonoidK`)
        - `Parallel[partial.Result]` (implementing also`NonEmptyParallel`)
        - `Semigroup[partial.Result.Errors]`
     - for `Codec` type class:
        - `Category[Codec]`
        - `InvariantSemigroupal[Codec[Domain, *]]` (implementing also `Invariant`, `Semigroupal`)
     - for `Iso` type class:
        - `Category[Iso]`
        - `InvariantSemigroupal[Iso[First, *]]` (implementing also `Invariant`, `Semigroupal`)
  - instances for `cats.data` types allowing Chimney to recognize them as collections:
    - `cats.data.Chain` (transformation _from_ and _to_ always available)
    - `cats.data.NonEmptyChain` (transformations: _from_ always available, _to_ only with `PartialTransformer`
      or to another `NonEmptyChain`)
    - `cats.data.NonEmptyLazyList` (transformation: _from_ always available, _to_ only with `PartialTransformer`
      or to another `NonEmptyLazyList`, the type is only defined on 2.13+)
    - `cats.data.NonEmptyList` (transformation: _from_ always available, _to_ only with `PartialTransformer`
      or to another `NonEmptyList`)
    - `cats.data.NonEmptyMap` (transformation: _from_ always available, _to_ only with `PartialTransformer`
      or to another `NonEmptyMap`)
    - `cats.data.NonEmptySeq` (transformation: _from_ always available, _to_ only with `PartialTransformer`
      or to another `NonEmptySeq`)
    - `cats.data.NonEmptySet` (transformation: _from_ always available, _to_ only with `PartialTransformer`
      or to another `NonEmptySet`)
    - `cats.data.NonEmptyVector` (transformation: _from_ always available, _to_ only with `PartialTransformer`
      or to another `NonEmptyVector`)
  - transforming `F[A]` to `G[B]` if implicit `F ~> G` and `Traverse[F]` are present

!!! important

    You need to import ``io.scalaland.chimney.cats._`` to have all of the above in scope.

### Conversion from/into Cats `Validated`

`Validated[E, A]` values can be converted into `partial.Result[A]` using `asResult` extension method, when their
`E` (`Invalid`) type is:

 - `partial.Result.Errors`
 - `NonEmptyChain[partial.Error]`
 - `NonEmptyList[partial.Error]`
 - `NonEmptyChain[String]`
 - `NonEmptyList[String]`

as soon as we import both `io.scalaland.chimney.partial.syntax._` (for extension method) and
`io.scalaland.chimney.cats._` (instances for `Validated`).

Just like you could run `asOption` or `asEither` on `PartialTransformer` result, you can convert to `Validated` with
new extension methods: `asValidatedNec`, `asValidatedNel`, `asValidatedChain` and `asValidatedList`.

!!! example

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class RegistrationForm(
        email: String,
        username: String,
        password: String,
        age: String
    )

    case class RegisteredUser(
        email: String,
        username: String,
        passwordHash: String,
        age: Int
    )

    import cats.data._
    import io.scalaland.chimney._
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.partial.syntax._
    import io.scalaland.chimney.cats._

    def hashpw(pw: String): String = s"trust me bro, $pw is hashed"

    def validateEmail(form: RegistrationForm): ValidatedNec[String, String] =
      if (form.email.contains('@')) {
        Validated.valid(form.email)
      } else {
        Validated.invalid(NonEmptyChain(s"${form.username}'s email: does not contain '@' character"))
      }

    def validateAge(form: RegistrationForm): ValidatedNec[String, Int] = form.age.toIntOption match {
      case Some(value) if value >= 18 => Validated.valid(value)
      case Some(value) => Validated.invalid(NonEmptyChain(s"${form.username}'s age: must have at least 18 years"))
      case None        => Validated.invalid(NonEmptyChain(s"${form.username}'s age: invalid number"))
    }

    implicit val partialTransformer: PartialTransformer[RegistrationForm, RegisteredUser] =
      PartialTransformer
        .define[RegistrationForm, RegisteredUser]
        .withFieldComputedPartial(_.email, form => validateEmail(form).asResult)
        .withFieldComputed(_.passwordHash, form => hashpw(form.password))
        .withFieldComputedPartial(_.age, form => validateAge(form).asResult)
        .buildTransformer

    val okForm = RegistrationForm("john@example.com", "John", "s3cr3t", "40")
    pprint.pprintln(
      okForm.transformIntoPartial[RegisteredUser].asValidatedNec
    )
    // expected output:
    // Valid(
    //   a = RegisteredUser(
    //     email = "john@example.com",
    //     username = "John",
    //     passwordHash = "trust me bro, s3cr3t is hashed",
    //     age = 40
    //   )
    // )

    pprint.pprintln(
      Array(
        RegistrationForm("john_example.com", "John", "s3cr3t", "10"),
        RegistrationForm("alice@example.com", "Alice", "s3cr3t", "19"),
        RegistrationForm("bob@example.com", "Bob", "s3cr3t", "21.5")
      ).transformIntoPartial[Array[RegisteredUser]].asValidatedNel
    )
    // expected output:
    // Invalid(
    //   e = NonEmptyList(
    //     head = Error(
    //       message = StringMessage(message = "John's email: does not contain '@' character"),
    //       path = Path(elements = List(Computed(targetPath = "_.email"), Index(index = 0)))
    //     ),
    //     tail = List(
    //       Error(
    //         message = StringMessage(message = "John's age: must have at least 18 years"),
    //         path = Path(elements = List(Computed(targetPath = "_.age"), Index(index = 0)))
    //       ),
    //       Error(
    //         message = StringMessage(message = "Bob's age: invalid number"),
    //         path = Path(elements = List(Computed(targetPath = "_.age"), Index(index = 2)))
    //       )
    //     )
    //   )
    // )
    ```

    Form validation logic is implemented in terms of `Validated` data type. You can easily convert
    it to a `partial.Result` required by `withFieldComputedPartial` by just using `.toPartialResult`
    which is available after importing the cats integration utilities (`import io.scalaland.chimney.cats._`).
    
    Result of the partial transformation is then converted to `ValidatedNel` or `ValidatedNec` using either
    `.asValidatedNel` or `.asValidatedNec` extension method call.

### Conversions to/from Cats collections

If you want to convert between Scala collections and Cats collections, or between 2 Cats collections
(or between Cats collections and some other collection whose support was provided via integration e.g. Java
collection), then you can:

 * convert *from* `Chain`, `NonEmptyChain`, `NonEmptyList`, `NonEmptyLazyList`, `NonEmptyMap`, `NonEmptySeq`,
   `NonEmptySet` and `NonEmptyVector` with *both* `Transformer`s and `PartialTransformer`s (since iterating over
   a collection is always possible)
 * convert *into* `Chain` with *both* `Transformer`s and `PartialTransformer`s (since `Chain` can always be created)
 * convert *into* `NonEmptyChain`, `NonEmptyList`, `NonEmptyLazyList`, `NonEmptyMap`, `NonEmptySeq` and `NonEmptySeq`,
   and `NonEmptySet` and `NonEmptyVector` with *only* `PartialTransformer`s (since their constructor performs
   validation), except when you try to
 * convert *between* `NonEmptyChain` and another `NonEmptyChain`, `NonEmptyList` and another `NonEmptyList`,
   `NonEmptyLazyList` and another `NonEmptyLazyList`, `NonEmptyMap` and another `NonEmptyMap`,
   `NonEmptySeq` and another `NonEmptySeq`, `NonEmptyVector` and another `NonEmptyVector`,
    and any other `F[A]` into `F[B]` which has `Traverse` instance, with *both* `Transformer`s and `PartialTransformer`s
    (since we can use `.traverseWithIndexM` and avoid running that validation again)
 * convert any collection `F[_]` that has `Traverse[F]` and between any 2 collections `F[_]`, `G[_]` if implicit
   `Traverse[F]` and `F ~> G` exist

!!! example "Converting from Cats collections"

   ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.data._
    import cats.Order
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.cats._

    case class Foo(a: Int)
    case class Bar(a: Int)

    pprint.pprintln(
      Chain.one(Foo(10)).transformInto[List[Bar]]
    )
    pprint.pprintln(
      NonEmptyChain.one(Foo(10)).transformInto[List[Bar]]
    )
    pprint.pprintln(
      NonEmptyList.one(Foo(10)).transformInto[List[Bar]]
    )
    implicit val fooOrder: Order[Foo] = Order.by[Foo, Int](_.a) // required by NonEmptySet.one!!!
    import Order.catsKernelOrderingForOrder // required by NonEmptySet integration!!!
    pprint.pprintln(
      NonEmptySet.one(Foo(10)).transformInto[List[Bar]]
    )
    pprint.pprintln(
      NonEmptyVector.one(Foo(10)).transformInto[List[Bar]]
    )
    // expected output:
    // List(Bar(a = 10))
    // List(Bar(a = 10))
    // List(Bar(a = 10))
    // List(Bar(a = 10))
    // List(Bar(a = 10))
    ```

!!! example "Converting into Cats collections"

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.data._
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.cats._

    case class Foo(a: Int)
    case class Bar(a: Int)

    pprint.pprintln(
      List(Foo(10)).transformInto[Chain[Bar]]
    )
    pprint.pprintln(
      List(Foo(10)).transformIntoPartial[NonEmptyChain[Bar]].asOption
    )
    pprint.pprintln(
      List(Foo(10)).transformIntoPartial[NonEmptyList[Bar]].asOption
    )
    implicit val barOrdering: Ordering[Bar] = Ordering.by[Bar, Int](_.a) // required by NonEmptySet integration!!!
    pprint.pprintln(
      List(Foo(10)).transformIntoPartial[NonEmptySet[Bar]].asOption
    )
    pprint.pprintln(
      List(Foo(10)).transformIntoPartial[NonEmptyVector[Bar]].asOption
    )
    // expected output:
    // Singleton(a = Bar(a = 10))
    // Some(value = Singleton(a = Bar(a = 10)))
    // Some(value = NonEmptyList(head = Bar(a = 10), tail = List()))
    // Some(value = TreeSet(Bar(a = 10)))
    // Some(value = NonEmptyVector(Bar(10)))
    ```

!!! example "Converting between Cats collections of the same type"

   ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.data._
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.cats._

    case class Foo(a: Int)
    case class Bar(a: Int)

    pprint.pprintln(
      Chain.one(Foo(10)).transformInto[Chain[Bar]]
    )
    pprint.pprintln(
      NonEmptyChain.one(Foo(10)).transformInto[NonEmptyChain[Bar]]
    )
    pprint.pprintln(
      NonEmptyList.one(Foo(10)).transformInto[NonEmptyList[Bar]]
    )
    pprint.pprintln(
      NonEmptyVector.one(Foo(10)).transformInto[NonEmptyVector[Bar]]
    )
    // expected output:
    // Singleton(a = Bar(a = 10))
    // Singleton(a = Bar(a = 10))
    // NonEmptyList(head = Bar(a = 10), tail = List())
    // NonEmptyVector(Bar(10))
    ```

!!! example "Converting using implicit ~> (FunctionK)"

   ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.~>
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.cats._

    implicit val intToStr: Transformer[Int, String] = _.toString
    implicit val listToOption: List ~> Option = new (List ~> Option) {
      def apply[A](fa: List[A]): Option[A] = fa.headOption
    }

    pprint.pprintln(
      List(1, 2, 3).transformInto[Option[String]]
    )
    // expected output:
    // Some(value = "1")
    ```

### Cats instances

If you have the experience with Cats and their type classes, then behavior of `Transformer` needs no additional
explanation:

!!! example

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.cats._

    val example: Transformer[Int, String] = _.toString

    pprint.pprintln(
      example.map(str => s"value is $str").transform(10)
    )
    pprint.pprintln(
      example.dimap[Double, String](_.toInt)(str => "value " + str).transform(10.50)
    )
    // example.contramap[Double](_.toInt).transform(10.50) // Scala has a problem inferring what is F and what is A here
    pprint.pprintln(
      cats.arrow.Arrow[Transformer].id[String].transform("value")
    )
    // expected output:
    // "value is 10"
    // "value 10"
    // "value"
    ```

Similarly, there exists instances for `PartialTransformer` and `partial.Result`: 

!!! example

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.cats._

    val example: PartialTransformer[String, Int] = PartialTransformer.fromFunction[String, Int](_.toInt)

    pprint.pprintln(
      example.map(int => int.toDouble).transform("10")
    )
    pprint.pprintln(
      example.dimap[String, Float](str => str + "00")(int => int.toFloat).transform("10")
    )
    pprint.pprintln(
      cats.arrow.Arrow[PartialTransformer].id[String].transform("value")
    )
    // expected output:
    // Value(value = 10.0)
    // Value(value = 1000.0F)
    // Value(value = "value")
    ```

However, between Cats and Chimney, there is a difference in approach when it comes to "sequential" and "parallel"
computations.
 
!!! note

    For this explanation we are assuming there we are using some type which represents computations that can be
    interrupted - e.g. exception was thrown (`Try`, `cats.effect.IO`), we got `Left` value (`Either`) or `Invalid`
    (`Validated`). In Chimney such type is `partial.Result`.

For starters, let us remember how such distinction is handled in Cats.

!!! info "How it works in Cats" 

     - when we are using thing like e.g. `map`, `flatMap` to chain the operations if any of these operations "fail", we
       are not running the operations that would follow
     - so, `Monad` is usually representing some sequential computations, which (for types representing fallible
       computations) can "fail fast" or "short circuit"
       - since `Applicative` extends `Monad` and their behavior should be compatible (= applicative operations are
         implemented using `flatMap` under the hood) then e.g.
         `(NonEmptyList.of("error1").asLeft[Int], NonEmptyList.of("error2".asLeft[Int])).tupled` would contain only one
         error (the first one, `NonEmptyList.of("error1")`), even though we used the type which could aggregate them
     - for such cases - where we want to always use all partial results - Cats prepared `Parallel` type class which
       would allow us to `(NonEmptyList.of("error1").asLeft[Int], NonEmptyList.of("error2".asLeft[Int])).parTupled`
       which would combine the results
       - it's quite important to remember that `Parallel` here doesn't mean "asynchronous operations that run
         concurrently" but "failure in one result doesn't discard other results before we start to combine them"
     - long story short: when seeing `map`, `flatMap`, `tupled`, `mapN` we should expect "sequential" semantics which
       "fail fast" and for aggregation of errors requires "parallel" semantics with operations like `parTupled`,
       `parMapN`, `parFlatMapN` - the semantics is chosen at compile time by the choice of operator we used

    It makes perfect sense, because - while these type class do NOT have to be used with IO monads - these type classes
    CAN be used with IO monads, and so `map`, `flatMap`, `parMapN`, etc can be used to express the "structured
    concurrency" without introducing separate type classes. The kind of semantics we need is known upfront and
    hardcoded.

Now, let's take a look what Chimney does, and why the behavior is different.

!!! info "How it works in Chimney"

     - we have `partial.Result` data type which can aggregate multiple errors into one value
     - however, this aggregation is costly, so if you don't need it you can pass `failFast: Boolean` parameter to
       `PartialTransformer` (e.g. `from.transformIntoPartial[To](failFast = true)`) or `partial.Result` utilities
       (e.g. `partial.Result.traverse(coll.toIterator, a => ..., failFast = true)`)
     - it means that Chimney decides which semantics to use in runtime, with a flag

    And it makes sense for Chimney: during a type class derivation we do not select the semantics, because semantics chosen
    in one place, would not work in another forcing users to derive everything twice. It also allows us to pass this flag
    from some config or context and for better debugging experience (e.g. using fail fast for normal operations, but letting
    us change a switch in the deployed app without recompiling and redeploying everything to test just one call).

What does it mean to us?

!!! warning

    It means that every `PartialTransformer` which internally combines results from several smaller partial
    transformations has to have both semantics implemented internally and switch between them with a flag. If we combine
    `PartialTransformer`s using Cats' type classes and extension methods, the type class instance
    can:

     - pass the `failFast: Boolean` flag on
     - use the flag to decide on semantics when combinding several smaller `partial.Result`s
     - however, some operations like `mapN` use `flatMap` under the hood, so while the flag is propagated, some results
       can still be discarded, and e.g. `parProduct` or `parMapN` would have to be used NOT to use parallel semantics
       but to NOT disable parallel semantics for some transformations when we would pass `failFast = false` later on
    
    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.cats._

    val t0 = PartialTransformer.fromFunction[String, Int](_.toInt)
    val t1 = t0.map(_.toDouble)
    val t2 = t0.map(_.toLong)

    // uses 1 input value to create a tuple of 2 values, fails fast on the error for the first
    pprint.pprintln(
      t1.product(t2).transform("aa").asEitherErrorPathMessageStrings
    )
    // expected output:
    // Left(value = List(("", "For input string: \"aa\"")))

    // uses 1 input value to create a tuple of 2 values, agregates the errors for both
    pprint.pprintln(
      t1.parProduct(t2).transform("aa").asEitherErrorPathMessageStrings
    )
    // expected output:
    // Left(value = List(("", "For input string: \"aa\""), ("", "For input string: \"aa\"")))
    ```

    And `partial.Result`s have to use explicit combinators to decide whether it's sequential or parallel semantics:

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.cats._

    val result1 = partial.Result.fromErrorString[Int]("error 1")
    val result2 = partial.Result.fromErrorString[Double]("error 2")

    // all of these will preserve only the first error ("error 1"):
    pprint.pprintln(
      (result1, result2).mapN((a: Int, b: Double) => a + b) // partial.Result[Double]
    )
    pprint.pprintln(
      result1.product(result2) // partial.Result[(Int, Double)]
    )
    pprint.pprintln(
      result1 <* result2 // partial.Result[Int]
    )
    pprint.pprintln(
      result1 *> result2 // partial.Result[Double]
    )
    // expected output:
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List()))
    //   )
    // )

    // all of these will preserve both errors:
    pprint.pprintln(
      (result1, result2).parMapN((a: Int, b: Double) => a + b) // partial.Result[Double]
    )
    pprint.pprintln(
      result1.parProduct(result2) // partial.Result[(Int, Double)]
    )
    pprint.pprintln(
      result1 <& result2 // partial.Result[Int]
    )
    pprint.pprintln(
      result1 &> result2 // partial.Result[Double]
    )
    // expected output:
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List())),
    //     Error(message = StringMessage(message = "error 2"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List())),
    //     Error(message = StringMessage(message = "error 2"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List())),
    //     Error(message = StringMessage(message = "error 2"), path = Path(elements = List()))
    //   )
    // )
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(message = StringMessage(message = "error 1"), path = Path(elements = List())),
    //     Error(message = StringMessage(message = "error 2"), path = Path(elements = List()))
    //   )
    // )
    ```

Notice that this is not an issue if we are transforming one value into another value in a non-fallible way, e.g. through
`map`, `contramap`, `dimap`. There is also no issie if we chain several `flatMap`s for something more like Kleisli
composition (`result.flatMap(f).flatMap(g)`) but becomes an issue when we use `flatMap` and `flatMap`-based operations
for building products (`result.flatMap(a => result2.map(b => (a, b))`).

Once we understand that difference we are able to understand the differences between building `PartialTransformer`s
and `partial.Result`s with `parMapN` and `for`-comprehension:

!!! example "Combining PartialTransformers with Cats"

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.cats._

    case class Foo(a: String, b: String)
    case class Bar(a: Int, b: Int)

    pprint.pprintln(
      (for {
        a <- PartialTransformer[Foo, Int](foo => partial.Result.fromCatching(foo.a.toInt))
        b <- PartialTransformer[Foo, Int](foo => partial.Result.fromCatching(foo.b.toInt))
      } yield Bar(a, b))
        .transform(Foo("a", "b"))
        .asErrorPathMessageStrings
    )
    // expected output:
    // List(("", "For input string: \"a\""))

    pprint.pprintln(
      (
        PartialTransformer[Foo, Int](foo => partial.Result.fromCatching(foo.a.toInt)),
        PartialTransformer[Foo, Int](foo => partial.Result.fromCatching(foo.b.toInt))
      ).parMapN((a, b) => Bar(a, b))
        .transform(Foo("a", "b"))
        .asErrorPathMessageStrings
    )
    // expected output:
    // List(("", "For input string: \"a\""), ("", "For input string: \"b\""))
    ```

!!! example "Combining partial.Results with Cats"

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.cats._

    case class Foo(a: String, b: String)
    case class Bar(a: Int, b: Int)
    
    pprint.pprintln(
      (for {
        a <- partial.Result.fromCatching("a".toInt)
        b <- partial.Result.fromCatching("b".toInt)
      } yield Bar(a, b))
        .asErrorPathMessageStrings
    )
    // expected output:
    // List(("", "For input string: \"a\""))

    pprint.pprintln(
      (
        partial.Result.fromCatching("a".toInt),
        partial.Result.fromCatching("b".toInt)
      ).parMapN((a, b) => Bar(a, b))
        .asErrorPathMessageStrings
    )
    // expected output:
    // List(("", "For input string: \"a\""), ("", "For input string: \"b\""))
    ```

`Transformer`s have only `mapN`/for-comprehension as they as there is nothing that they can aggregate:

!!! example "Combining Transformers with Cats"

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.Transformer
    import io.scalaland.chimney.cats._

    case class Foo(a: String, b: String)
    case class Bar(a: Int, b: Int)

    pprint.pprintln(
      (for {
        a <- cats.arrow.Arrow[Transformer].lift[Bar, String](_.a.toString)
        b <- cats.arrow.Arrow[Transformer].lift[Bar, String](_.a.toString)
      } yield Foo(a, b))
        .transform(Bar(10, 20))
    )
    // expected output:
    // Foo(a = "10", b = "10")

    pprint.pprintln(
      (
        cats.arrow.Arrow[Transformer].lift[Bar, String](_.a.toString),
        cats.arrow.Arrow[Transformer].lift[Bar, String](_.a.toString)
      ).mapN((a, b) => Foo(a, b))
        .transform(Bar(10, 20))
    )
    // expected output:
    // Foo(a = "10", b = "10")
    ```

Piping `Transformer`s/`PartialTransformers` is also possible:

!!! example "Piping Transformers with Cats"

    ```scala
    //> using dep org.typelevel::cats-core::{{ libraries.cats }}
    //> using dep io.scalaland::chimney-cats::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import cats.syntax.all._
    import io.scalaland.chimney.{PartialTransformer, Transformer}
    import io.scalaland.chimney.cats._

    case class Foo(a: Int)

    pprint.pprintln(
      cats.arrow.Arrow[Transformer].lift[Foo, Int](_.a)
        .andThen(cats.arrow.Arrow[Transformer].lift[Int, String](_.toString))
        .transform(Foo(10))
    )
    // expected output:
    // "10"

    pprint.pprintln(
      cats.arrow.Arrow[PartialTransformer].lift[Int, String](_.toString)
        .compose(cats.arrow.Arrow[PartialTransformer].lift[Foo, Int](_.a))
        .transform(Foo(10))
        .asOption
    )
    // expected output:
    // Some(value = "10")
    ```

## Protocol Buffers integration

Most of the time, working with Protocol Buffers should not be different from
working with any other DTO objects. `Transformer`s could be used to encode
domain objects into protobufs and `PartialTransformer`s could decode them.

However, there are 2 concepts specific to PBs and their implementation in
ScalaPB: storing unencoded values in an additional case class field and
wrapping done by sealed traits' cases in `oneof` values.

### `UnknownFieldSet`

By default, ScalaPB would generate in a case class an additional field
`unknownFields: UnknownFieldSet = UnknownFieldSet()`. This field
could be used if you want to somehow log/trace some extra values -
perhaps from another version of the schema - were passed but your current
version's parser did not need it.

The automatic conversion into a protobuf with such a field can be problematic:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    object scalapb {
      case class UnknownFieldSet()
    }

    object domain {
      case class Address(line1: String, line2: String)
    }
    object protobuf {
      case class Address(
        line1: String = "",
        line2: String = "",
        unknownFields: scalapb.UnknownFieldSet = scalapb.UnknownFieldSet()
      )
    }

    domain.Address("a", "b").transformInto[protobuf.Address]
    // expected error:
    // Chimney can't derive transformation from domain.Address to protobuf.Address
    //
    // protobuf.Address
    //   unknownFields: scalapb.UnknownFieldSet - no accessor named unknownFields in source type domain.Address
    //
    // Consult https://scalalandio.github.io/chimney for usage examples.
    ```

There are 2 ways in which Chimney could handle this issue:

  - using [default values](supported-transformations.md#allowing-fallback-to-the-constructors-default-values)
  
    !!! example "Globally enabled default values"
  
        ```scala
        domain
          .Address("a", "b")
          .into[protobuf.Address]
          .enableDefaultValues
          .transform

        // can be set up for the whole scope with: 
        // implicit val cfg = TransformerConfiguration.default.enableDefaultValues
        ```
  
    !!! example "Default values scoped only to UnknownFieldSet"
  
        ```scala
        domain
          .Address("a", "b")
          .into[protobuf.Address]
          .enableDefaultValueOfType[scalapb.UnknownFieldSet]
          .transform
        // can be set up for the whole scope with: 
        // implicit val cfg = TransformerConfiguration.default.enableDefaultValueOfType[scalapb.UnknownFieldSet]
        ```

  - manually [setting this one field](supported-transformations.md#wiring-the-constructors-parameter-to-a-provided-value)_

    !!! example

        ```scala
        domain
          .Address("a", "b")
          .into[protobuf.Address]
          .withFieldConst(_.unknownFields, UnknownFieldSet())
          .transform
        ```

However, if you have the control over the ScalaPB generation process, you could configure it
to simply not generate this field, either
by [editing the protobuf](https://scalapb.github.io/docs/customizations#file-level-options):

!!! example

    ```protobuf
    option (scalapb.options) = {
      preserve_unknown_fields: false
    };
    ```

or adding to [package-scoped options](https://scalapb.github.io/docs/customizations#package-scoped-options).
If the field won't be generated in the first place, there will be no issues with providing values to it.

At this point, one might also consider another option:

!!! example

    ```protobuf
    option (scalapb.options) = {
      no_default_values_in_constructor: true
    };
    ```

preventing ScalaBP from generating default values in constructor, to control
how exactly the protobuf value is created.

### `oneof` fields

`oneof` is a way in which Protocol Buffers allows using ADTs. The example PB:

!!! example

    ```protobuf
    message AddressBookType {
      message Public {}
      message Private {
        string owner = 1;
      }
      oneof value {
        Public public = 1;
        Private private = 2;
      }
    }
    ```

would generate scala code similar to (some parts removed for brevity):

!!! example

    ```scala
    package pb.addressbook

    final case class AddressBookType(
        value: AddressBookType.Value = AddressBookType.Value.Empty
    ) extends scalapb.GeneratedMessage
        with scalapb.lenses.Updatable[AddressBookType] {
      // ...
    }

    object AddressBookType extends scalapb.GeneratedMessageCompanion[AddressBookType] {
      sealed trait Value extends scalapb.GeneratedOneof
      object Value {
        case object Empty extends AddressBookType.Value {
          // ...
        }
        final case class Public(value: AddressBookType.Public) extends AddressBookType.Value {
          // ...
        }
        final case class Private(value: AddressBookType.Private) extends AddressBookType.Value {
          // ...
        }
      }
      final case class Public(
      ) extends scalapb.GeneratedMessage
          with scalapb.lenses.Updatable[Public] {}

      final case class Private(
          owner: _root_.scala.Predef.String = ""
      ) extends scalapb.GeneratedMessage
          with scalapb.lenses.Updatable[Private] {
        // ...
      }

      // ...
    }
    ```

As we can see:

  - there is an extra `Value.Empty` type
  - this is not a "flat" `sealed` hierarchy - `AddressBookType` wraps sealed hierarchy `AddressBookType.Value`,
    where each `case class` wraps the actual message

!!! warning

    This is the default output, there are 2 other (opt-in) possibilities described below!

Meanwhile, we would like to extract it into a flat:

!!! example

    ```scala
    package addressbook

    sealed trait AddressBookType
    object AddressBookType {
      case object Public extends AddressBookType
      case class Private(owner: String) extends AddressBookType
    }
    ```

Luckily for us, since 0.8.x Chimney supports
[automatic (un)wrapping of sealed hierarchy cases](supported-transformations.md#non-flat-adts).

Encoding (with `Transformer`s) is pretty straightforward:

!!! example

    ```scala
    import io.scalaland.chimney.dsl._

    val domainType: addressbook.AddressBookType = addressbook.AddressBookType.Private("test")
    val pbType: pb.addressbook.AddressBookType =
      pb.addressbook.AddressBookType.of(
        pb.addressbook.AddressBookType.Value.Private(
          pb.addressbook.AddressBookType.Private.of("test")
        )
      )

    domainType.into[pb.addressbook.AddressBookType.Value].transform == pbType.value
    ```

Decoding (with `PartialTransformer`s) requires handling of `Empty.Value` type

  - we can do it manually:
  
    !!! example
    
        ```scala
        import io.scalaland.chimney.dsl._

        pbType.value
          .intoPartial[addressbook.AddressBookType]
          .withSealedSubtypeHandledPartial[pb.addressbook.AddressBookType.Value.Empty.type](
            _ => partial.Result.fromEmpty
          )
          .transform
          .asOption == Some(domainType)
        ```

  - or handle all such fields with a single import:

    !!! example
  
        ```scala
        import io.scalaland.chimney.dsl._
        import io.scalaland.chimney.protobufs._ // includes support for empty scalapb.GeneratedOneof

        pbType.value.intoPartial[addressbook.AddressBookType].transform.asOption == Some(domainType)
        ```

!!! warning

    Importing `import io.scalaland.chimney.protobufs._` works only for the default output. If you used `sealed_value` or
    `sealed_value_optional` read further sections. 

!!! notice

    As you may have notices transformation is between `addressbook.AddressBookType` and
    `pb.addressbook.AddressBookType.Value`. If we wanted to automatically wrap/unwrap
    `pb.addressbook.AddressBookType.Value` with `pb.addressbook.AddressBookType` we should
    [enable non-AnyVal wrapper types](supported-transformations.md#frominto-a-wrapper-type).

    ```scala
    // enable unwrapping/wrapping inline
    domainType.into[pb.addressbook.AddressBookType].enableNonAnyValWrappers.transform == pbType

    locally {
      // enable unwrapping/wrapping for all derivations in the scope
      implicit val cfg = TransformerConfiguration.default.enableNonAnyValWrappers

      domainType.transformInto[pb.addressbook.AddressBookType] == pbType
    }
    ```

### `sealed_value oneof` fields

In case we can edit our protobuf definition, we can arrange the generated code
to be a flat `sealed` hierarchy. It requires fulfilling [several conditions defined by ScalaPB](https://scalapb.github.io/docs/sealed-oneofs#sealed-oneof-rules).
For instance, the code below follows the mentioned requirements:

!!! example
  
    ```protobuf
    message CustomerStatus {
      oneof sealed_value {
        CustomerRegistered registered = 1;
        CustomerOneTime oneTime = 2;
      }
    }

    message CustomerRegistered {}
    message CustomerOneTime {}
    ```

and it would generate something like (again, some parts omitted for brevity):

!!! example
  
    ```scala
    package pb.order

    sealed trait CustomerStatus extends scalapb.GeneratedSealedOneof {
      type MessageType = CustomerStatusMessage
    }

    object CustomerStatus {
      case object Empty extends CustomerStatus
      sealed trait NonEmpty extends CustomerStatus
    }

    final case class CustomerRegistered(
    ) extends scalapb.GeneratedMessage
        with CustomerStatus.NonEmpty
        with scalapb.lenses.Updatable[CustomerRegistered] {
      // ...
    }

    final case class CustomerOneTime(
    ) extends scalapb.GeneratedMessage
        with CustomerStatus.NonEmpty
        with scalapb.lenses.Updatable[CustomerOneTime] {
      // ...
    }
    ```

Notice, that while this implementation is flat, it still adds `CustmerStatus.Empty` - it happens because this type
would be used directly inside the message that contains is, and it would be non-nullable (while the `oneof`
content could still be absent).

Transforming to and from:

!!! example
  
    ```scala
    package order

    sealed trait CustomerStatus
    object CustomerStatus {
      case object CustomerRegistered extends CustomerStatus
      case object CustomerOneTime extends CustomerStatus
    }
    ```

could be done

  - manually

    !!! example
      
        ```scala
        import io.scalaland.chimney.dsl._
    
        val domainStatus: order.CustomerStatus = order.CustomerStatus.CustomerRegistered
        val pbStatus: pb.order.CustomerStatus = pb.order.CustomerRegistered()
    
        domainStatus.into[pb.order.CustomerStatus].transform == pbStatus
    
        pbStatus
          .intoPartial[order.CustomerStatus]
          .withSealedSubtypeHandledPartial[pb.order.CustomerStatus.Empty.type](
            _ => partial.Result.fromEmpty
          )
          .withSealedSubtypeHandled[pb.order.CustomerStatus.NonEmpty](
            _.transformInto[order.CustomerStatus]
          )
          .transform
          .asOption == Some(domainStatus)
        ```

  - or with an import

    !!! example
  
        ```scala
        import io.scalaland.chimney.dsl._
        import io.scalaland.chimney.protobufs._ // includes support for empty scalapb.GeneratedSealedOneof
      
        val domainStatus: order.CustomerStatus = order.CustomerStatus.CustomerRegistered
        val pbStatus: pb.order.CustomerStatus = pb.order.CustomerRegistered()
      
        pbStatus.transformIntoPartial[order.CustomerStatus].asOption == Some(domainStatus)
        ```

### `sealed_value_optional oneof` fields

If instead of a non-nullable type with `.Empty` subtype, we prefer `Option`al type without `.Empty` subtype, there is
an optional sealed hierarchy available. Similarly to a non-optional it requires [several conditions](https://scalapb.github.io/docs/sealed-oneofs#optional-sealed-oneof).

When you define message according to them:

!!! example
  
    ```protobuf
    message PaymentStatus {
      oneof sealed_value_optional {
        PaymentRequested requested = 1;
        PaymentCreated created = 2;
        PaymentSucceeded succeeded = 3;
        PaymentFailed failed = 4;
      }
    }

    message PaymentRequested {}
    message PaymentCreated {
      string external_id = 1;
    }
    message PaymentSucceeded {}
    message PaymentFailed {}
    ```

and try to map it to and from:

!!! example
  
    ```scala
    package order

    sealed trait PaymentStatus
    object PaymentStatus {
      case object PaymentRequested extends PaymentStatus
      case class PaymentCreated(externalId: String) extends PaymentStatus
      case object PaymentSucceeded extends PaymentStatus
      case object PaymentFailed extends PaymentStatus
    }
    ```

the transformation is pretty straightforward in both directions:

!!! example
  
    ```scala
    val domainStatus: Option[order.PaymentStatus] = Option(order.PaymentStatus.PaymentRequested)
    val pbStatus: Option[pb.order.PaymentStatus] = Option(pb.order.PaymentRequested())

    domainStatus.into[Option[pb.order.PaymentStatus]].transform == pbStatus
    pbStatus.into[Option[order.PaymentStatus]].transform == domainStatus
    ```

since there is no `Empty` case to handle. Wrapping into `Option` would
be handled automatically, similarly unwrapping (as long as you decode using
partial transformers).

### enum fields

ScalaPB turn enum fields into wrapped sealed traits, with additional `Unrecognized` value (all the other values are
subtypes of `Recognized` sealed subtrait). E.g:

!!! example

    ```protobuf
    enum PhoneType {
      MOBILE = 0;
      HOME = 1;
      WORK = 2;
    }
    ```

will generate something similar to:

!!! example

    ```scala
    sealed abstract class PhoneType(val value: Int) extends scalapb.GeneratedEnum
    
    object PhoneType extends scalapb.GeneratedEnumCompanion[PhoneType] {
    
      sealed trait Recognized extends PhoneType
      
      case object MOBILE extends PhoneType(0) with PhoneType.Recognized { /* ... */ }
      case object HOME extends PhoneType(0) with PhoneType.Recognized { /* ... */ }
      case object WORK extends PhoneType(0) with PhoneType.Recognized { /* ... */ }
      
      final case class Unrecognized(unrecognizedValue: Int) extends PhoneType(unrecognizedValue)
        with scalapb.UnrecognizedEnum
        
      // ...
    }
    ```

conversion to and from:

!!! example

    ```scala
    sealed trait PhoneType
    object PhoneType {
      case object MOBILE extends PhoneType
      case object HOME extends PhoneType
      case object WORK extends PhoneType
    }
    ```

could be done

  - manually

    !!! example

        ```scala
        import io.scalaland.chimney.dsl._
  
        val domainType: PhoneType = PhoneType.MOBILE
        val pbType: pb.addressbook.PhoneType = pb.addressbook.PhoneType.MOBILE
  
        domainType.transformInto[pb.addressbook.PhoneType] == pbType

        pbType
          .intoPartial[addressbook.PhoneType]
          .withEnumCaseHandledPartial[pb.addressbook.PhoneType.Unrecognized](_ => partial.Result.fromEmpty)
          .transform == domainType
        ```

  - or with an import

    !!! example

        ```scala
        import io.scalaland.chimney.dsl._
        import io.scalaland.chimney.protobufs._ // includes support for empty scalapb.GeneratedEnum
  
        val domainType: PhoneType = PhoneType.MOBILE
        val pbType: pb.addressbook.PhoneType = pb.addressbook.PhoneType.MOBILE
  
        domainType.transformInto[pb.addressbook.PhoneType] == pbType
        pbType.transformIntoPartial[addressbook.PhoneType].transform == domainType
        ```

### Build-in ScalaPB types

There are several datatypes provided by ScalaBP (or Google PB) which are not automatically converted into Scala's types,
that Chimney could convert for you:

  - `com.google.protobuf.empty.Empty` into `scala.Unit`
  - anything into `com.google.protobuf.empty.Empty`
  - `com.google.protobuf.duration.Duration` from/into `java.time.Duration`/`java.time.FiniteDuration`
  - `com.google.protobuf.timestamp.Timestamp` from/to `java.time.Instant`
  - `com.google.protobuf.ByteString` from/to any Scala collections of `Byte`s
  - wrapping/unwrapping Scala primitives with/from: 
    - `com.google.protobuf.wrappers.BoolValue` (`Boolean`)
    - `com.google.protobuf.wrappers.BytesValue` (collection of `Byte`s)
    - `com.google.protobuf.wrappers.DoubleValue` (`Double`)
    - `com.google.protobuf.wrappers.Int32Value` (`Int`)
    - `com.google.protobuf.wrappers.Int64Value` (`Long`)
    - `com.google.protobuf.wrappers.UInt32Value` (`Int`)
    - `com.google.protobuf.wrappers.UInt64Value` (`Long`)
    - `com.google.protobuf.wrappers.StringValue` (`String`)

Each of these transformations is provided by the same import:

!!! example

    ```scala
    //> using dep io.scalaland::chimney-protobufs::{{ chimney_version() }}
    import io.scalaland.chimney.protobufs._
    ```

## Changing naming conventions of fields decoded from JSON

Matching/generation of JSON field name is always done in runtime, which makes it relatively easy for JSON
libraries to let user inject their configuration: it's just a pure function that works on `String`/`List[String]`
and it can be defined even next to the codec that would use it. That's why you can simply define `def`/`val`
and pass it into e.g. `implicit` the `Configuration` in Circe, or as an argument for `CodecMaker.make` in Jsoniter. 

It harder to provide such function for a macro to run it during compilation: macro can only call code compiled
into the bytecode and available in class path, so such function would have to be defined in another module,
compiler before the module that would have to use it. That's the reason behing limitation of
[custom name comparison](supported-transformations.md#customizing-field-name-matching) in Chimney.

That's why the most straightforward and recommended way of converting the name convention is by:

  * having a dedicated type for domain operations/business logic
  * having a dedicated type for decoding JSONs into, *reflecting the expected JSON schema*
  * *using the same names* for those fields that should be each other's direct counterparts
  * providing the name convention converter for JSON decoding library
  * providing the overrides for Chimney transformers, minimizing the amout of customizations by having matching name conventions

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep io.circe::circe-generic-extras::0.14.4
    //> using dep io.circe::circe-parser::0.14.10
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class Foo(someName: String, anotherName: Int)
    case class Bar(someName: String, anotherName: Int, extra: Option[Double])

    import io.circe.{Encoder, Decoder}
    import io.circe.generic.extras.Configuration
    import io.circe.generic.extras.semiauto._
    import io.circe.parser.decode
    import io.circe.syntax._

    // Here we're configuring the convention conversion...
    implicit val customConfig: Configuration =
      Configuration.default.withKebabCaseMemberNames

    implicit val fooDecoder: Decoder[Foo] = deriveConfiguredDecoder[Foo]
    implicit val fooEncoder: Encoder[Foo] = deriveConfiguredEncoder[Foo]

    import io.scalaland.chimney.dsl._

    // ...so that we don't need to do it here:

    pprint.pprintln(
      decode[Foo]("""{ "some-name": "value", "another-name": 10 }""").toOption
        .map(_.into[Bar].enableOptionDefaultsToNone.transform)
    )
    // expected output:
    // Some(value = Bar(someName = "value", anotherName = 10, extra = None))

    pprint.pprintln(
      Bar("value", 10, None).transformInto[Foo].asJson
    )
    // expected output:
    // JObject(value = object[some-name -> "value",another-name -> 10])
    ```

This isn't always possible. One might not be able to use it e.g. when:

  * case classes are not controlled by the developer but generated by some codegen
  * case classes are provided by some external dependency
  * JSON library at use does not provide an ability to customize the naming convention conversion
  * etc

In such case, Chimney can still match names with different conventions, although the user would have to provide a function
which would compare them according to [custom name comparison requirements](supported-transformations.md#customizing-field-name-matching).

!!! example

    Name comparison which has to be defined in a separate module:

    ```scala
    // file: your/organization/KebabNamesComparison.scala - part of custom naming comparison example
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep io.circe::circe-generic::0.14.10
    //> using dep io.circe::circe-parser::0.14.10
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    package your.organization

    import io.scalaland.chimney.dsl._

    case object KebabNamesComparison extends TransformedNamesComparison {

      private def normalize(name: String): String = 
        if (name.contains('-')) {
          val head :: tail = name.split('-').filter(_.nonEmpty).toList: @unchecked
          head + tail
            .map(segment => s"${segment.head.toUpper}${segment.tail.toLowerCase}")
            .mkString
        } else name

      def namesMatch(fromName: String, toName: String): Boolean =
        normalize(fromName) == normalize(toName)
    }
    ```

    Module with name comparison has to be a dependency of the module which needs it to match field names:

    ```scala
    // file: your/organization/KebabNamesComparison.test.scala - part of custom naming comparison example
    //> using dep org.scalameta::munit::1.0.0

    case class Foo(`some-name`: String, `another-name`: Int)
    case class Bar(someName: String, anotherName: Int, extra: Option[Double])

    class Test extends munit.FunSuite {
      test("should compile") {
        import io.circe.{Encoder, Decoder}
        import io.circe.generic.semiauto._
        import io.circe.parser.decode
        import io.circe.syntax._

        implicit val fooDecoder: Decoder[Foo] = deriveDecoder[Foo]
        implicit val fooEncoder: Encoder[Foo] = deriveEncoder[Foo]

        import io.scalaland.chimney.dsl._

        implicit val cfg = TransformerConfiguration.default
          .enableCustomFieldNameComparison(your.organization.KebabNamesComparison)

        pprint.pprintln(
          decode[Foo]("""{ "some-name": "value", "another-name": 10 }""").toOption
            .map(_.into[Bar].enableOptionDefaultsToNone.transform)
        )
        // expected output:
        // Some(value = Bar(someName = "value", anotherName = 10, extra = None))

        pprint.pprintln(
          Bar("value", 10, None).transformInto[Foo].asJson
        )
        // expected output:
        // JObject(value = object[some-name -> "value",another-name -> 10])
      }
    }
    ```

## Encoding/decoding sealed/enum with `String`

Out of the box Chimney does not encode `sealed trait`/`enum` value as `String` and it does not decode `String`
as `partial.Result` of `sealed trait`/`enum`. But you can easily do it yourself!
(Or use [Enumz](https://enumz.readthedocs.io/) integration).

!!! example "Scala 2/3 with sealed/Java enum/Scala 3 enum/Enumeration"

    If you don't mind adding an additional dependency `enumz-chimney` would handle a total transformation from
    enum to String and a partial transformation from String to enum with just 1 import:

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep io.scalaland::enumz-chimney::{{ libraries.enumz }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.{Transformer, PartialTransformer}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.partial.syntax._

    // this import handles all the cases
    import io.scalaland.enumz.chimney._

    sealed trait Foo extends Product with Serializable
    object Foo {
      case object Bar extends Foo
      case object Baz extends Foo
    }
      
    pprint.pprintln(
      (Foo.Bar : Foo).transformInto[String]
    )
    // expected output:
    // "Bar"
    pprint.pprintln(
      "Bar".transformIntoPartial[Foo]
    )
    // expected output:
    // Value(value = Bar)
    pprint.pprintln(
      "Foo".transformIntoPartial[Foo]
    )
    // expected output:
    // Errors(errors = NonEmptyErrorsChain(Error(message = EmptyValue, path = Path(elements = List()))))
    ```

    However, if you don't want to add a dependency - or if you want to customize how `String` is encoded/decoded -
    the examples below could give you an idea.

!!! warning

    Enumz Chimney integration provides implicits to encode/decode `String` but also overrides the default way enums
    are handled with Chimney. That means that e.g. [customizing subtype name matching](supported-transformations.md#customizing-subtype-name-matching)
    will no longer work. We suggest importing it selectively, only when and where needed.

!!! example "Scala 2 with Enumeratum"

    If you are already using [Enumeratum](https://github.com/lloydmeta/enumeratum), you can use it to define your
    transformers. You can adapt this approach to make it work for `StringEnumEntry`/`IntEnumEntry`/etc.

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.beachape::enumeratum::{{ libraries.enumeratum }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import enumeratum._
    import io.scalaland.chimney.{Transformer, PartialTransformer}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.partial.syntax._

    sealed trait Foo extends EnumEntry
    object Foo extends Enum[Foo] {
      case object Bar extends Foo
      case object Baz extends Foo

      def values = findValues
    }

    locally {
      // hardcoded version, working with Foo specifically
      implicit val encodeFoo: Transformer[Foo, String] =
        new Transformer[Foo, String] {
  
          def transform(src: Foo): String =
            src.toString
        }
      implicit val decodeFoo: PartialTransformer[String, Foo] =
        new PartialTransformer[String, Foo] {
  
          def transform(src: String, failFast: Boolean): partial.Result[Foo] =
            Foo.withNameEither(src).left.map(_.getMessage).asResult
        }
        
      pprint.pprintln(
        (Foo.Bar : Foo).transformInto[String]
      )
      // expected output:
      // "Bar"
      pprint.pprintln(
        "Bar".transformIntoPartial[Foo]
      )
      // expected output:
      // Value(value = Bar)
      pprint.pprintln(
        "Foo".transformIntoPartial[Foo]
      )
      // expected output:
      // Errors(
      //   errors = NonEmptyErrorsChain(
      //     Error(
      //       message = StringMessage(message = "Foo is not a member of Enum (Bar, Baz)"),
      //       path = Path(elements = List())
      //     )
      //   )
      // )
    }

    locally {
      // generic version, working with every EnumEntry
      implicit def encode[E <: EnumEntry: Enum]: Transformer[E, String] =
        new Transformer[E, String] {
  
          def transform(src: E): String =
            src.toString
        }
      implicit def decoder[E <: EnumEntry: Enum]: PartialTransformer[String, E] =
        new PartialTransformer[String, E] {
  
          def transform(src: String, failFast: Boolean): partial.Result[E] =
            implicitly[Enum[E]].withNameEither(src).left.map(_.getMessage).asResult
        }
        
      pprint.pprintln(
        (Foo.Bar : Foo).transformInto[String]
      )
      // expected output:
      // "Bar"
      pprint.pprintln(
        "Bar".transformIntoPartial[Foo]
      )
      // expected output:
      // Value(value = Bar)
      pprint.pprintln(
        "Foo".transformIntoPartial[Foo]
      )
      // expected output:
      // Errors(
      //   errors = NonEmptyErrorsChain(
      //     Error(
      //       message = StringMessage(message = "Foo is not a member of Enum (Bar, Baz)"),
      //       path = Path(elements = List())
      //     )
      //   )
      // )
    }
    ```

!!! example "Scala 3 enum with parameterless cases"

    ```scala
    //> using scala {{ scala.3 }}
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.{Transformer, PartialTransformer}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.partial.syntax._

    enum Foo {
      case Bar, Baz
    }

    implicit val encodeFoo: Transformer[Foo, String] =
      new Transformer[Foo, String] {

        def transform(src: Foo): String =
          src.toString
      }
    implicit val decodeFoo: PartialTransformer[String, Foo] =
      new PartialTransformer[String, Foo] {

        def transform(src: String, failFast: Boolean): partial.Result[Foo] =
          scala.util.Try(Foo.valueOf(src)).asResult
      }
      
    pprint.pprintln(
      (Foo.Bar : Foo).transformInto[String]
    )
    // expected output:
    // "Bar"
    pprint.pprintln(
      "Bar".transformIntoPartial[Foo]
    )
    // expected output:
    // Value(value = Bar)
    pprint.pprintln(
      "Foo".transformIntoPartial[Foo]
    )
    // expected output:
    // Errors(
    //   errors = NonEmptyErrorsChain(
    //     Error(
    //       message = ThrowableMessage(
    //         throwable = java.lang.IllegalArgumentException: enum snippet$_.Foo has no case with name: Foo
    //       ),
    //       path = Path(elements = List())
    //     )
    //   )
    // )
    ```

!!! example "Scala with sealed trait"

    `Enum` type class from [Enumz](https://github.com/scalalandio/enumz) works for:
    
      - `sealed trait`s/`sealed abstract class`es (including Enumeratum ones)
      - Java `enum`s
      - Scala 3 `enum`s
      - Scala `Enumeration` type

    so you can use it to define transformers for each of these cases.
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep io.scalaland::enumz::{{ libraries.enumz }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.{Transformer, PartialTransformer}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.partial.syntax._

    import io.scalaland.enumz.Enum

    sealed trait Foo extends Product with Serializable
    object Foo {
      case object Bar extends Foo
      case object Baz extends Foo
    }

    // hardcoded version, working with Foo specifically
    implicit val encodeFoo: Transformer[Foo, String] =
      new Transformer[Foo, String] {

        def transform(src: Foo): String =
          src.toString
      }
    implicit val decoderFoo: PartialTransformer[String, Foo] =
      new PartialTransformer[String, Foo] {

        def transform(src: String, failFast: Boolean): partial.Result[Foo] =
          Enum[Foo].withNameOption(src).asResult
      }

    // generic version, working with every enum type
    implicit def encode[E: Enum]: Transformer[E, String] =
      new Transformer[E, String] {

        def transform(src: E): String =
          Enum[E].getName(src)
      }
    implicit def decode[E: Enum]: PartialTransformer[String, E] =
      new PartialTransformer[String, E] {

        def transform(src: String, failFast: Boolean): partial.Result[E] =
          Enum[E].withNameOption(src).asResult
      }
      
    pprint.pprintln(
      (Foo.Bar : Foo).transformInto[String]
    )
    // expected output:
    // "Bar"
    pprint.pprintln(
      "Bar".transformIntoPartial[Foo]
    )
    // expected output:
    // Value(value = Bar)
    pprint.pprintln(
      "Foo".transformIntoPartial[Foo]
    )
    // expected output:
    // Errors(errors = NonEmptyErrorsChain(Error(message = EmptyValue, path = Path(elements = List()))))
    ```

## Lens-like use cases

Chimney can be used in some cases where optics/prisms are normally used. Let us demonstrate them by reimplementing
some [Quicklens](https://github.com/softwaremill/quicklens) example, where we would update a value of nested case
classes.

!!! example

    Let' set we have a nested structure:
    
    ```scala
    case class Foo(bar: Option[Bar])
    case class Bar(baz: List[Baz])
    case class Baz(a: Int, b: String, c: Double)
    
    val foo = Foo(
      bar = Some(
        Baz(
          baz = List(
            Baz(a = 1, b = "a", c = 10.0),
            Baz(a = 2, b = "b", c = 20.0)
          )
        )
      )
    )
    ```
    
    Let's say we need to update all `a` in Baz to `10` and all `b` to `"new"`. With Quicklens we could implement it like
    this:
    
    ```scala
    //> using dep com.softwaremill.quicklens::quicklens::{{ libraries.quicklens }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    case class Foo(bar: Option[Bar])
    case class Bar(baz: List[Baz])
    case class Baz(a: Int, b: String, c: Double)
    
    val foo = Foo(
      bar = Some(
        Bar(
          baz = List(
            Baz(a = 1, b = "a", c = 10.0),
            Baz(a = 2, b = "b", c = 20.0)
          )
        )
      )
    )
    
    import com.softwaremill.quicklens._
  
    pprint.pprintln(
      foo
        .modify(_.bar.each.baz.each.a).setTo(10)
        .modify(_.bar.each.baz.each.b).setTo("new")
    )
    // expected output:
    // Foo(
    //   bar = Some(
    //     value = Bar(baz = List(Baz(a = 10, b = "new", c = 10.0), Baz(a = 10, b = "new", c = 20.0)))
    //   )
    // )
    ```
    
    It could be translated to Chimney like this:
    
    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    case class Foo(bar: Option[Bar])
    case class Bar(baz: List[Baz])
    case class Baz(a: Int, b: String, c: Double)
    
    val foo = Foo(
      bar = Some(
        Bar(
          baz = List(
            Baz(a = 1, b = "a", c = 10.0),
            Baz(a = 2, b = "b", c = 20.0)
          )
        )
      )
    )
    
    import io.scalaland.chimney.dsl._
    
    pprint.pprintln(
      foo
        .into[Foo]
        .withFieldConst(_.bar.matchingSome.baz.everyItem.a, 10)
        .withFieldConst(_.bar.matchingSome.baz.everyItem.b, "new")
        .enableMacrosLogging
        .transform
    )
    // expected output:
    // Foo(
    //   bar = Some(
    //     value = Bar(baz = List(Baz(a = 10, b = "new", c = 10.0), Baz(a = 10, b = "new", c = 20.0)))
    //   )
    // )
    ```

Some comparison between the two could be found in the table below:

| Quicklens                              | Chimney                                                            |
|----------------------------------------|--------------------------------------------------------------------|
| `value.modify(path).setTo(fieldValue)` | `value.into[ValueType].withFieldConst(path, fieldValue).transform` |
| `.fieldName`                           | `.fieldName`                                                       |
| `.each` (collection, non-`Map`)        | `.everyItem`                                                       |
| `.each` (collection, `Map`)            | `.everyMapValue`                                                   |
| `.each` (`Option`)                     | `.matchingSome`                                                    |
| `.eachLeft`                            | `.matchingLeft`                                                    |
| `.eachRight`                           | `.matchingRight`                                                   |
| `.when[Subtype]`                       | `.matching[Subtype]`                                               |

Additionally, Chimney defines `.everyMapKey`.

There are no Chimney counterparts for Quicklens':

 * `.at(idx)`/`.at(key)` (update specific index/map key, throwing if absent)
 * `.index(idx)` (update specific index/map key, ignoring if absent)
 * `.atOrElse(idx, value)` (update specific index/map key, using `value` if absent)
 * `.eachWhere(predicate)` (update all items fulfilling the predicate)
 * `.setToIf(predicate)(value)` (updates if predicate is fulfilled)
 * `.setToIfDefined(option)` (updates using `Option`)
 * `.using(f)` (update the field with specific fun)

For these cases, a proper optics library (like Quicklens) is recommended. As you can see method names in Chimney DSL
were selected in such way that there should be no conflicts with other libraries, so you don't have to choose one - you
can pick up both.

## Patching `case class` with another instance of the same `case class`

You can use 2 instances of the same `case class` to copy fields form one another - you only need to exclude some
fields from the patching:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    import io.scalaland.chimney.dsl._

    case class Foo(a: String, b: String)

    pprint.pprintln(
      Foo("a", "b").using(Foo("c", "d"))
        .withFieldIgnored(_.a)
        .patch
    )
    // expected output:
    // Foo(a = "a", b = "d")
    ```

## Patching optional field with value decoded from JSON

JSON cannot define a nested optional values - since there is no wrapper like `Some` there is no way to represent difference between
`Some(None)` and `None` using build-in JSON semantics. If during `POST` request one want to always use `Some` values to **update**,
and `None` values to always indicate *keep old* semantics **or** always indicate *clear value* semantics (if the modified value is `Option` as well),
this is enough.

The problem, arises when one wantes to express 3 possible outcomes for modifying an `Option` value: *update value*/*keep old*/*clear value*.

The only solution in such case is to express in the API the 3 possible outcomes somwhow without resorting to nested `Option`s. As long as it can
be done, the type can be converted to nested `Option`s which have unambguous semantics:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    //> using dep io.circe::circe-generic-extras::0.14.4
    //> using dep io.circe::circe-parser::0.14.10
    import io.circe.{Encoder, Decoder}
    import io.circe.generic.extras.Configuration
    import io.circe.generic.extras.auto._
    import io.circe.generic.extras.semiauto._
    import io.circe.parser.decode
    import io.circe.syntax._

    // An example of representing set-clean-keep operations in a way that cooperates with JSONs.
    sealed trait OptionalUpdate[+A] extends Product with Serializable {

      def toOption: Option[Option[A]] = this match {
        case OptionalUpdate.Set(value) => Some(Some(value))
        case OptionalUpdate.Clear      => Some(None)
        case OptionalUpdate.Keep       => None
      }
    }
    object OptionalUpdate {

      case class Set[A](value: A) extends OptionalUpdate[A]
      case object Clear extends OptionalUpdate[Nothing]
      case object Keep extends OptionalUpdate[Nothing]

      private implicit val customConfig: Configuration =
        Configuration.default
          .withDiscriminator("action")
          .withSnakeCaseConstructorNames

      implicit def encoder[A: Encoder]: Encoder[OptionalUpdate[A]] =
        deriveConfiguredEncoder
      implicit def decoder[A: Decoder]: Decoder[OptionalUpdate[A]] =
        deriveConfiguredDecoder
    }

    case class Foo(field: Option[String], anotherField: String)

    case class FooUpdate(field: OptionalUpdate[String])
    object FooUpdate {

      private implicit val customConfig: Configuration = Configuration.default
      implicit val encoder: Encoder[FooUpdate] = deriveConfiguredEncoder
      implicit val decoder: Decoder[FooUpdate] = deriveConfiguredDecoder
    }

    import io.scalaland.chimney.Patcher
    import io.scalaland.chimney.dsl._

    // This utility allows to automatically handle Option patching with OptionalUpdate values.
    implicit def patchWithOptionalUpdate[A, Patch](implicit
        inner: Patcher.AutoDerived[Option[A], Option[Option[A]]]
    ): Patcher[Option[A], OptionalUpdate[A]] = (obj, patch) =>
      obj.patchUsing(patch.toOption)

    pprint.pprintln(
      decode[FooUpdate](
        """{ "field": { "action": "set", "value": "new-value" } }"""
      ) match {
        case Left(error)  => println(error)
        case Right(patch) => Foo(Some("old-value"), "another-value").patchUsing(patch)
      }
    )
    // expected output:
    // Foo(field = Some(value = "new-value"), anotherField = "another-value")
    pprint.pprintln(
      decode[FooUpdate](
        """{ "field": { "action": "clear" } }"""
      ) match {
        case Left(error)  => println(error)
        case Right(patch) => Foo(Some("old-value"), "another-value").patchUsing(patch)
      }
    )
    // expected output:
    // Foo(field = None, anotherField = "another-value")
    pprint.pprintln(
      decode[FooUpdate](
        """{ "field": { "action": "keep" } }"""
      ) match {
        case Left(error)  => println(error)
        case Right(patch) => Foo(Some("old-value"), "another-value").patchUsing(patch)
      }
    )
    // expected output:
    // Foo(field = Some(value = "old-value"), anotherField = "another-value")
    ```

If we cannot modify our API, we have to [choose one semantics for `None` values](supported-patching.md#treating-none-as-no-update-instead-of-set-to-none).

## Mixing Scala 2.13 and Scala 3 types

[Scala 2.13 project can use Scala 3 artifacts and vice versa](https://docs.scala-lang.org/scala3/guides/migration/compatibility-classpath.html).
For Scala 3 project to depends on Scala 2.13 usually only some build tool configuration is needed, e.g.

```scala
libraryDependencies += ("org.bar" %% "bar" % "1.0.0").cross(CrossVersion.for3Use2_13)
```

For Scala 2.13 to rely on Scala 3 artifact, and additional compiler option is required as well:

```scala
libraryDependencies += ("org.bar" %% "bar" % "1.0.0").cross(CrossVersion.for2_13Use3)
scalacOptions += "-Ytasty-reader"
```

One can even create module hierarchies where Scala 3 module depends on 2.13, which depends on 3, which depends on 2.13,
etc., sometimes called [the sandwich pattern](https://www.scala-lang.org/blog/2021/04/08/scala-3-in-sbt.html#the-sandwich-pattern).  

Chimney took it seriously to make sure that in such case:

 * Scala 2.13 macros would be able to handle `case class`es and `sealed trait`s compiled with Scala 3  
 * Scala 3 macros would be able to handle `case class`es and `sealed trait`s compiled with Scala 2.13
 * `@BeanProperty` would continue working despite
   [changes in their semantics](https://docs.scala-lang.org/scala3/guides/migration/incompat-other-changes.html#invisible-bean-property)
 * default values will also work despite [changes to constructors](https://docs.scala-lang.org/scala3/reference/other-new-features/creator-applications.html)
   which changed how default values are stored in the byte code 

to contribute to easier migration from Scala 2.13 to Scala 3.

!!! note

    At the moment conversion from Scala 3 `enum` by Scala 2.13 macros is not yet handled correctly.
    
!!! warning

    Chimney is NOT [mixing Scala 2.13 and Scala 3 macros](https://docs.scala-lang.org/scala3/guides/migration/tutorial-macro-mixing.html)
    and probably never will.
    
    It is required that there is only 1 version of Chimney on the class-path - either Scala 2 or Scala 3 version - which
    would be called only from modules with the matching version of Scala.

## Integrations

While Chimney supports a lot of transformations out of the box, sometimes it needs our help. We can do it ad hoc
like described in [Supported transformations](supported-transformations.md), but if we are maintaining some library
we would like our users to be able to integrate with Chimney using a single import. How to do it?

Transformations between 2 fully-known types can be handled with normal `implicit` values:

```scala
// Foo is a proper type
// Bar is a proper type
implicit val fooBarTransformer: Transformer[Foo, Bar] = ...
```

The problem arises when we need some genericness. E.g. if we wanted to provide a transformation between collections:

```scala
// CollectionFoo[A] is our own collection type parametrized with A
// CollectionBar[B] is our own collection type parametrized with B
implicit def fooCollectionBarCollectionTransformer[A, B](
  implicit abBar: Transformer[A, B]
): Transformer[FooCollection[A], BarCollection[B]] = ...
```

such generic `implicit` would:

 * NOT autoderive `Transformer` even if Chimney could do it - for that we would have to use `Transformer.AutoDerived`
   (why is explained in [one of sections above](#automatic-semiautomatic-and-inlined-derivation))
 * NOT cooperate with DSL for overriding values by paths e.g.
   `FooCollection[Foo].into[BarCollection[Bar]].withFieldConst(_.everyItem.value, someValue).transform`
 * require defining a separate `implicit` between each 2 collections types

Similarly, newtypes/refined types would require dedicated pair of implicits for wrapping/unwrapping if we went with
a naive approach, custom optional types would not behave like `Option`s, etc.

To make integration with libraries easier we prepared this section as well as a dedicated package in Chimney
namespace: `io.scalaland.chimney.integrations`. Examples of integrations provided this way are
[Cats](#cats-integration), [Java collections](#java-collections-integration) and [Protobufs](#protocol-buffers-integration)
modules.

### Libraries with smart constructors

Any type that uses a smart constructor (returning parsed result rather than throwing an exception) would require
Partial Transformer rather than Total Transformer to convert.

If there is no common interface that could be summoned as implicit for performing smart construction:

!!! example

    Assuming Scala 3 or `-Xsource:3` for fixed `private` constructors so that `Username.apply` and `.copy` would
    be private. (Newest versions of Scala 2.13 additionally require us to acknowledge this change in the behavior by
    manually suppressing an error/warning).

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Xsource:3 -Wconf:cat=scala3-migration:s
    final case class Username private (value: String)
    object Username {
      def parse(value: String): Either[String, Username] =
        if (value.isEmpty) Left("Username cannot be empty")
        else Right(Username(value))
    }
    ```

    ```scala
    //> using scala {{ scala.3 }}
    final case class Username private (value: String)
    object Username {
      def parse(value: String): Either[String, Username] =
        if value.isEmpty then Left("Username cannot be empty")
        else Right(Username(value))
    }
    ```

then Partial Transformer would have to be created manually:

!!! example

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Xsource:3 -Wconf:cat=scala3-migration:s
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    
    final case class Username private (value: String)
    object Username {
      def parse(value: String): Either[String, Username] =
        if (value.isEmpty) Left("Username cannot be empty")
        else Right(Username(value))
    }
    
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.partial

    implicit val usernameParse: PartialTransformer[String, Username] =
      PartialTransformer[String, Username] { value =>
        partial.Result.fromEitherString(Username.parse(value))
      }
    ```

However, if there was some type class interface, e.g.

!!! example

    ```scala
    trait SmartConstructor[From, To] {
      def parse(from: From): Either[String, To]
    }
    ```

!!! example

    ```scala
    object Username extends SmartConstructor[String, Username] {
      // ...
    }
    ```

we could use it to construct `PartialTransformer` automatically:

!!! example

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Xsource:3 -Wconf:cat=scala3-migration:s
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.PartialTransformer
    import io.scalaland.chimney.partial

    trait SmartConstructor[From, To] {
      def parse(from: From): Either[String, To]
    }

    implicit def smartConstructedPartial[From, To](implicit
        smartConstructor: SmartConstructor[From, To]
    ): PartialTransformer[From, To] =
      PartialTransformer[From, To] { value =>
        partial.Result.fromEitherString(smartConstructor.parse(value))
      }
      
    final case class Username private (value: String)
    object Username extends SmartConstructor[String, Username] {
      def parse(value: String): Either[String, Username] =
        if (value.isEmpty) Left("Username cannot be empty")
        else Right(Username(value))
    }
    ```

The same would be true about extracting values from smart-constructed types
(if they are not ``AnyVal``\s, handled by Chimney out of the box).

Let's see how we could implement support for automatic transformations of
types provided in some popular libraries.

#### Scala NewType

[NewType](https://github.com/estatico/scala-newtype) is a macro-annotation-based
library which attempts to remove runtime overhead from user's types.

!!! example

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Ymacro-annotations
    //> using dep io.estatico::newtype::0.4.4
    import io.estatico.newtype.macros.newtype

    @newtype case class Username(value: String)
    ```

would be rewritten to become `String` in the runtime, while prevent
mixing `Username` values with other `String`s accidentally.

NewType provides `Coercible` type
class [to allow generic wrapping and unwrapping](https://github.com/estatico/scala-newtype#coercible-instance-trick)
of `@newtype` values. This type class is not able to validate the cast type, so it is safe to use only if NewType is used
as a wrapper around another type that performs this validation e.g. Refined Type.

!!! example

    ```scala
    //> using scala {{ scala.2_13 }}
    //> using options -Ymacro-annotations
    //> using dep io.estatico::newtype::0.4.4
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.estatico.newtype.Coercible
    import io.scalaland.chimney.Transformer

    implicit def newTypeTransformer[From, To](implicit
        coercible: Coercible[From, To]
    ): Transformer[From, To] = coercible(_)
    ```

#### Monix Newtypes

[Monix's Newtypes](https://newtypes.monix.io/) is similar to NewType in that
it tries to remove wrapping in runtime. However, it uses different tricks
(and syntax) to achieve it.

!!! example

    ```scala
    //> using dep io.monix::newtypes-core::0.2.3
    import monix.newtypes._

    type Username = Username.Type
    object Username extends NewtypeValidated[String] {
      def apply(value: String): Either[BuildFailure[Type], Type] =
        if (value.isEmpty) Left(BuildFailure("Username cannot be empty"))
        else Right(unsafeCoerce(value))
    }
    ```

Additionally, it provides 2 type classes: one to extract value
(`HasExtractor`) and one to wrap it (possibly validating, `HasBuilder`).
We can use them to provide unwrapping `Transformer` and wrapping
`PartialTransformer`:

!!! example

    ```scala
    //> using dep io.monix::newtypes-core::0.2.3
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.{PartialTransformer, Transformer}
    import io.scalaland.chimney.partial
    import monix.newtypes._

    implicit def unwrapNewType[Outer, Inner](implicit
        extractor: HasExtractor.Aux[Outer, Inner]
    ): Transformer[Outer, Inner] = extractor.extract(_)

    implicit def wrapNewType[Inner, Outer](implicit
        builder: HasBuilder.Aux[Outer, Inner]
    ): PartialTransformer[Inner, Outer] = PartialTransformer[Inner, Outer] { value =>
      partial.Result.fromEitherString(
        builder.build(value).left.map(_.toReadableString)
      )
    }
    ```

#### Refined Types

[Refined Types](https://github.com/fthomas/refined) is a library aiming to provide automatic validation of some
popular constraints as long as we express them in the value's type.

!!! example

    ```scala
    //> using dep eu.timepit::refined::0.11.1
    import eu.timepit.refined._
    import eu.timepit.refined.api.Refined
    import eu.timepit.refined.auto._
    import eu.timepit.refined.collection._

    type Username = String Refined NonEmpty
    ```

We can validate using the dedicated type class (`Validate`), while extraction is a simple accessor:

!!! example

    ```scala
    //> using dep eu.timepit::refined::0.11.1
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import eu.timepit.refined.refineV
    import eu.timepit.refined.api.{Refined, Validate}
    import io.scalaland.chimney.{PartialTransformer, Transformer}
    import io.scalaland.chimney.partial

    implicit def extractRefined[Type, Refinement]: Transformer[Type Refined Refinement, Type] =
      _.value

    implicit def validateRefined[Type, Refinement](implicit
      validate: Validate.Plain[Type, Refinement]
    ): PartialTransformer[Type, Type Refined Refinement] =
      PartialTransformer[Type, Type Refined Refinement] { value =>
        partial.Result.fromEitherString(refineV[Refinement](value))
      }
    ```

### Custom default values

If you are providing integration for a type which you do not control, and you'd like to let your users fall back
to default values when using Chimney, but the type does not define them - it might be still possible to provide them
with `io.scalaland.chimney.integrations.DefaultValue`. It could look like this:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    // Types which we cannot simply edit: come from external library, codegen, etc.
    
    case class MyType(int: Int)
    case class Foo(a: Int)
    case class Bar(a: Int, b: MyType)
    
    // Our integration:
    implicit val defaultMyType: io.scalaland.chimney.integrations.DefaultValue[MyType] = () => MyType(0)
    
    // Remember that default values has to be enabled!
    import io.scalaland.chimney.dsl._
    pprint.pprintln(
      Foo(10).into[Bar].enableDefaultValues.transform
    )
    pprint.pprintln(
      Foo(10).into[Bar].enableDefaultValueOfType[MyType].transform
    )
    // expected outputs:
    // Bar(a = 10, b = MyType(int = 0))
    // Bar(a = 10, b = MyType(int = 0))
    ``` 

Keep in mind, that such provision works for every constructor which has an argument of such type not matched with source
value, so it's only safe to use when in the scope which sees such implicit all derivations would only need default value
of this type, rather than convert it from something else.

### Custom optional types

In case your library/domain defines custom Optional types, you can provide your own handling of such types through
`io.scalaland.chimney.integrations.OptionalValue`. It could look like this:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    // When you define your own optional...
    sealed trait MyOptional[+A]
    object MyOptional {
      case class Present[+A](value: A) extends MyOptional[A]
      case object Absent extends MyOptional[Nothing]
      
      def apply[A](value: A): MyOptional[A] = if (value != null) Present(value) else Absent
    }
    
    // ...you can provide Chimney support for it...
    import io.scalaland.chimney.integrations.OptionalValue

    implicit def myOptionalIsOptionalValue[A]: OptionalValue[MyOptional[A], A] = new OptionalValue[MyOptional[A], A] {
      override def empty: MyOptional[A] = MyOptional.Absent
      // to match Option's' behavior, it should handle nulls
      override def of(value: A): MyOptional[A] = MyOptional(value)
      override def fold[A0](oa: MyOptional[A], onNone: => A0, onSome: A => A0): A0 = oa match {
        case MyOptional.Present(value) => onSome(value)
        case MyOptional.Absent         => onNone
      }
    }
    
    // ...so you could use it:
    import io.scalaland.chimney.dsl._
    
    // for converting between Option and custom optional type
    pprint.pprintln(
      Option("test").transformInto[MyOptional[String]]
    )
    pprint.pprintln(
      MyOptional("test").transformInto[Option[String]]
    )
    // expected output:
    // Present(value = "test")
    // Some(value = "test")
    
    // for automatinc wrapping with custom optional type
    pprint.pprintln(
      "test".transformInto[MyOptional[String]]
    )
    // expected output:
    // Present(value = "test")
    
    // for safe unwrapping with PartialTransformers
    pprint.pprintln(
      MyOptional("test").transformIntoPartial[String].asOption
    )
    pprint.pprintln(
      MyOptional("test").transformIntoPartial[String].asOption
    )
    // expected output:
    // Some(value = "test")
    // Some(value = "test")
    
    case class Foo(value: String)
    case class Bar(value: String, another: Double)
    
    // for overriding values with path to optional value like with Option
    pprint.pprintln(
      MyOptional(Foo("test"))
        .into[MyOptional[Bar]]
        .withFieldConst(_.matchingSome.another, 3.14)
        .transform
    )
    // expected output:
    // Present(value = Bar(value = "test", another = 3.14))
    ```

As you can see, once you provide 1 implicit your custom optional type:

 * can be converted to/from `scala.Option` (and other optional types)
 * can automatically wrap value
 * can automatically unwrap value in `PartialTransformer`s
 * can be used with `matchingSome` path in `withFieldConst`/`withFieldComputed`/etc

An example of such optional type is `java.util.Optional` for which support is provided via `OptionalValue` in
[Java collections' integration](#java-collections-integration).

### Custom collection types

In case your library/domain defines custom collections - which are:

 * NOT providing `scala.collection.Factory` (2.13/3) or `scala.collection.genric.CanBuildFrom` (2.12)
 * or NOT extending `Iterable`

you have to provide some configuration to help Chimney work with them.

Most of the time a collection doesn't perform any sort of validations, and you can always put items in it:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    // When you define your own collection...
    class MyCollection[+A] private (private val impl: Vector[A]) {
  
      def iterator: Iterator[A] = impl.iterator
  
      override def equals(obj: Any): Boolean = obj match {
        case myCollection: MyCollection[?] => impl == myCollection.impl
        case _                             => false
      }
      override def hashCode(): Int = impl.hashCode()
      override def toString: String = impl.mkString("MyCollection(", ", ", ")")
    }
    object MyCollection {
  
      def of[A](as: A*): MyCollection[A] = new MyCollection(as.toVector)
      def from[A](vector: Vector[A]): MyCollection[A] = new MyCollection(vector)
    }
    
    // ...you can provide Chimney support for it...
    import io.scalaland.chimney.integrations.{ FactoryCompat, TotallyBuildIterable }
    import scala.collection.compat._
    import scala.collection.mutable

    implicit def myCollectionIsTotallyBuildIterable[A]: TotallyBuildIterable[MyCollection[A], A] =
      new TotallyBuildIterable[MyCollection[A], A] {
        // Factory for your type
        def totalFactory: Factory[A, MyCollection[A]] = new FactoryCompat[A, MyCollection[A]] {
  
          override def newBuilder: mutable.Builder[A, MyCollection[A]] =
            new FactoryCompat.Builder[A, MyCollection[A]] {
              private val implBuilder = Vector.newBuilder[A]
  
              override def clear(): Unit = implBuilder.clear()
  
              override def result(): MyCollection[A] = MyCollection.from(implBuilder.result())
  
              override def addOne(elem: A): this.type = { implBuilder += elem; this }
            }
        }
  
        // your type as Iterator
        override def iterator(collection: MyCollection[A]): Iterator[A] = collection.iterator
      }
      
    // ...so you could use it:
    import io.scalaland.chimney.dsl._
    
    // for converting to and from standard library collection (or any other type supported this way)
    pprint.pprintln(
      MyCollection.of("a", "b").transformInto[List[String]]
    )
    pprint.pprintln(
      List("a", "b").transformInto[MyCollection[String]]
    )
    // expected output:
    // List("a", "b")
    // MyCollection(a, b)
    
    case class Foo(value: String)
    case class Bar(value: String, another: Double)
    
    // for overriding values with path to items like with standard library's collections
    pprint.pprintln(
      List(Foo("test"))
        .into[MyCollection[Bar]]
        .withFieldConst(_.everyItem.another, 3.14)
        .transform
    )
    // expected output:
    // MyCollection(Bar(test,3.14))
    ```

!!! tip

    If you are not sure whether the derivation treats your case as custom collection, [try enabling macro logging](troubleshooting.md#debugging-macros).

If your collection performs some sort of validation, you can integrate it with Chimney as well:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    // When you define your own collection...
    class NonEmptyCollection[+A] private (private val impl: Vector[A]) {
    
      def iterator: Iterator[A] = impl.iterator
  
      override def equals(obj: Any): Boolean = obj match {
        case nonEmptyCollection: NonEmptyCollection[?] => impl == nonEmptyCollection.impl
        case _                                         => false
      }
      override def hashCode(): Int = impl.hashCode()
      override def toString: String = impl.mkString("NonEmptyCollection(", ", ", ")")
    }
    object NonEmptyCollection {
  
      def of[A](a: A, as: A*): NonEmptyCollection[A] = new NonEmptyCollection(a +: as.toVector)
      def from[A](vector: Vector[A]): Option[NonEmptyCollection[A]] =
        if (vector.nonEmpty) Some(new NonEmptyCollection(vector)) else None
    }
    
    // ...you can provide Chimney support for it...
    import io.scalaland.chimney.integrations.{ FactoryCompat, PartiallyBuildIterable }
    import io.scalaland.chimney.partial
    import scala.collection.compat._
    import scala.collection.mutable

    implicit def nonEmptyCollectionIsPartiallyBuildIterable[A]: PartiallyBuildIterable[NonEmptyCollection[A], A] =
      new PartiallyBuildIterable[NonEmptyCollection[A], A] {
  
        // notice, that this Factory returns partial.Result of your collection!
        def partialFactory: Factory[A, partial.Result[NonEmptyCollection[A]]] =
          new FactoryCompat[A, partial.Result[NonEmptyCollection[A]]] {
  
            override def newBuilder: mutable.Builder[A, partial.Result[NonEmptyCollection[A]]] =
              new FactoryCompat.Builder[A, partial.Result[NonEmptyCollection[A]]] {
                private val implBuilder = Vector.newBuilder[A]
  
                override def clear(): Unit = implBuilder.clear()
  
                override def result(): partial.Result[NonEmptyCollection[A]] =
                  partial.Result.fromOption(NonEmptyCollection.from(implBuilder.result()))
  
                override def addOne(elem: A): this.type = { implBuilder += elem; this }
              }
          }
  
        override def iterator(collection: NonEmptyCollection[A]): Iterator[A] = collection.iterator
      }
      
    // ...so you could use it:
    import io.scalaland.chimney.dsl._
    
    // for validating that your collection can be created once all items have been put into Builder
    pprint.pprintln(
      List("a").transformIntoPartial[NonEmptyCollection[String]].asOption
    )
    pprint.pprintln(
      List.empty[String].transformIntoPartial[NonEmptyCollection[String]].asOption
    )
    // expected output:
    // Some(value = NonEmptyCollection(a))
    // None
    ```
    
For map types there are specialized versions of these type classes:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    
    import io.scalaland.chimney.integrations._
    import io.scalaland.chimney.partial
    import scala.collection.compat._
    import scala.collection.mutable

    class MyMap[+K, +V] private (private val impl: Vector[(K, V)]) {
  
      def iterator: Iterator[(K, V)] = impl.iterator
  
      override def equals(obj: Any): Boolean = obj match {
        case customMap: MyMap[?, ?] => impl == customMap.impl
        case _                          => false
      }
      override def hashCode(): Int = impl.hashCode()
    }
    object MyMap {
  
      def of[K, V](pairs: (K, V)*): MyMap[K, V] = new MyMap(pairs.toVector)
      def from[K, V](vector: Vector[(K, V)]): MyMap[K, V] = new MyMap(vector)
    }
  
    implicit def customMapIsTotallyBuildMap[K, V]: TotallyBuildMap[MyMap[K, V], K, V] =
      new TotallyBuildMap[MyMap[K, V], K, V] {
  
        def totalFactory: Factory[(K, V), MyMap[K, V]] = new FactoryCompat[(K, V), MyMap[K, V]] {
  
          override def newBuilder: mutable.Builder[(K, V), MyMap[K, V]] =
            new FactoryCompat.Builder[(K, V), MyMap[K, V]] {
              private val implBuilder = Vector.newBuilder[(K, V)]
  
              override def clear(): Unit = implBuilder.clear()
  
              override def result(): MyMap[K, V] = MyMap.from(implBuilder.result())
  
              override def addOne(elem: (K, V)): this.type = { implBuilder += elem; this }
            }
        }
  
        override def iterator(collection: MyMap[K, V]): Iterator[(K, V)] = collection.iterator
      }
  
    class NonEmptyMap[+K, +V] private (private val impl: Vector[(K, V)]) {
  
      def iterator: Iterator[(K, V)] = impl.iterator
  
      override def equals(obj: Any): Boolean = obj match {
        case nonEmptyMap: NonEmptyMap[?, ?] => impl == nonEmptyMap.impl
        case _                              => false
      }
      override def hashCode(): Int = impl.hashCode()
    }
    object NonEmptyMap {
  
      def of[K, V](pair: (K, V), pairs: (K, V)*): NonEmptyMap[K, V] = new NonEmptyMap(pair +: pairs.toVector)
      def from[K, V](vector: Vector[(K, V)]): Option[NonEmptyMap[K, V]] =
        if (vector.nonEmpty) Some(new NonEmptyMap(vector)) else None
    }
  
    implicit def nonEmptyMapIsPartiallyBuildMap[K, V]: PartiallyBuildMap[NonEmptyMap[K, V], K, V] =
      new PartiallyBuildMap[NonEmptyMap[K, V], K, V] {
  
        def partialFactory: Factory[(K, V), partial.Result[NonEmptyMap[K, V]]] =
          new FactoryCompat[(K, V), partial.Result[NonEmptyMap[K, V]]] {
  
            override def newBuilder: mutable.Builder[(K, V), partial.Result[NonEmptyMap[K, V]]] =
              new FactoryCompat.Builder[(K, V), partial.Result[NonEmptyMap[K, V]]] {
                private val implBuilder = Vector.newBuilder[(K, V)]
  
                override def clear(): Unit = implBuilder.clear()
  
                override def result(): partial.Result[NonEmptyMap[K, V]] =
                  partial.Result.fromOption(NonEmptyMap.from(implBuilder.result()))
  
                override def addOne(elem: (K, V)): this.type = { implBuilder += elem; this }
              }
          }
  
        override def iterator(collection: NonEmptyMap[K, V]): Iterator[(K, V)] = collection.iterator
      }
    ```
    
The only 2 difference they make is that:

 - when we are converting with `PartialTransformer` failures will be reported on map keys instead of `_1` and `_2` field
   of a tuple in a sequence (e.g. `keys(myKey)` - if key conversion failed for `myKey` value or `(myKey)` if value
   conversion failed for `myKey` key, instead of `(0)._1` or `(0)._2`)
 - they allow usage of `everyMapKey` and `everyMapValue` in paths, just like with standard library's `Maps`.

An example of such collections are `java.util` collections for which support is provided via `TotallyBuildIterable` 
and `TotallyBuildMap` in [Java collections' integration](#java-collections-integration), or `cats.data` types
provided in [Cats integration](#cats-integration).

### Custom outer type conversion

Providing `implicit` `Transformer` or `PartialTransformer` is usually needed when conversion between outer types
can be generated, except for some inner values. What if the opposite is true?

!!! example

    ```scala
    case class NonEmptyList[A] private (head: A, tail: List[A])
    object NonEmptyList { def make[A](a: A, as: A*): NonEmptyList[A] = new NonEmptyList(a, as.toList) }
    case class NonEmptyVector[A] private (head: A, tail: Vector[A])
    object NonEmptyVector { def make[A](a: A, as: A*): NonEmptyVector[A] = new NonEmptyVector(a, as.toVector) }
    ```

In the above example, we might want to convert `NonEmptyList` into `NonEmptyVector`. If we use integrations for
collections, then we can define `PartiallyBuildIterable` for both of them... but the conversion can only be partial,
with a `PartialTransformer[NonEmptyList[From], NonEmptyVector[To]]`. Even when we know that all such conversions
succeeds (we can always convert `From` into `To`), and that `NonEmptyList` when converted can only create a non-empty
vector -  `PartiallyBuildIterable` cannot know this. But we do.

Such outer conversion can be defined using `TotalOuterTransformer`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    import io.scalaland.chimney.integrations._
    import io.scalaland.chimney.partial
    
    case class NonEmptyList[A] private (head: A, tail: List[A])
    object NonEmptyList { def make[A](a: A, as: A*): NonEmptyList[A] = new NonEmptyList(a, as.toList) }
    case class NonEmptyVector[A] private (head: A, tail: Vector[A])
    object NonEmptyVector { def make[A](a: A, as: A*): NonEmptyVector[A] = new NonEmptyVector(a, as.toVector) }
    
    // Always creates NonEmptyVector as long as ALL of its values can be created
    implicit def nonEmptyListToNonEmptyVector[A, B]: TotalOuterTransformer[NonEmptyList[A], NonEmptyVector[B], A, B] =
      new TotalOuterTransformer[NonEmptyList[A], NonEmptyVector[B], A, B] {
      
        // used when A => B will be resolved by Chimney to be a total transformation
        def transformWithTotalInner(
            src: NonEmptyList[A],
            inner: A => B
        ): NonEmptyVector[B] = NonEmptyVector.make(inner(src.head), src.tail.map(inner).toSeq: _*)
      
        // used when A => B will be resolved by Chimney to be a partial transformation
        def transformWithPartialInner(
            src: NonEmptyList[A],
            failFast: Boolean,
            inner: A => partial.Result[B]
        ): partial.Result[NonEmptyVector[B]] = partial.Result.map2(
          inner(src.head),
          partial.Result.traverse[Seq[B], A, B](src.tail.iterator, inner, failFast),
          (head: B, tail: Seq[B]) => NonEmptyVector.make[B](head, tail: _*),
          failFast
        )
      }
      
    // ...and now we can convert:
    import io.scalaland.chimney.dsl._
    
    pprint.pprintln(
      NonEmptyList.make("a", "b").transformInto[NonEmptyVector[String]]
    )
    // expected output:
    // NonEmptyVector(head = "a", tail = Vector("b"))
    pprint.pprintln(
      NonEmptyList.make("a", "b").into[NonEmptyVector[String]]
        .withFieldConst(_.everyItem, "c") // we can provide overrides using .everyItem in DSL
        .transform
    )
    // expected output:
    // NonEmptyVector(head = "c", tail = Vector("c"))
    ```

The other kind of Outer Transformer is `PartialOuterTransformer`:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    import io.scalaland.chimney.integrations._
    import io.scalaland.chimney.partial
    
    case class NonEmptyList[A] private (head: A, tail: List[A])
    object NonEmptyList { def make[A](a: A, as: A*): NonEmptyList[A] = new NonEmptyList(a, as.toList) }
    case class TwoItemVector[A](head: A, tail: A)
    
    // Only creates TwoItemVector if NonEmptyList has exactly 2 items, and both are valid
    implicit def nonEmptyListToTwoItemVector[A, B]: PartialOuterTransformer[NonEmptyList[A], TwoItemVector[B], A, B] =
      new PartialOuterTransformer[NonEmptyList[A], TwoItemVector[B], A, B] {
      
        // used when A => B will be resolved by Chimney to be a total transformation
        def transformWithTotalInner(
            src: NonEmptyList[A],
            failFast: Boolean,
            inner: A => B
        ): partial.Result[TwoItemVector[B]] = src match {
          case NonEmptyList(a, b :: Nil) => partial.Result.fromValue(
            TwoItemVector[B](inner(a), inner(b))
          )
          case _ => partial.Result.fromErrorString("Exactly 2 items expected")
        }
      
        // used when A => B will be resolved by Chimney to be a partial transformation
        def transformWithPartialInner(
            src: NonEmptyList[A],
            failFast: Boolean,
            inner: A => partial.Result[B]
        ): partial.Result[TwoItemVector[B]] = src match {
          case NonEmptyList(a, b :: Nil) => partial.Result.map2(
            inner(a),
            inner(b),
            TwoItemVector[B](_, _),
            failFast
          )
          case _ => partial.Result.fromErrorString("Exactly 2 items expected")
        }
      }
      
    // ...and now we can convert:
    import io.scalaland.chimney.dsl._
    
    pprint.pprintln(
      NonEmptyList.make("a", "b").transformIntoPartial[TwoItemVector[String]]
    )
    // expected output:
    // Value(value = TwoItemVector(head = "a", tail = "b"))
    pprint.pprintln(
      NonEmptyList.make("a", "b").intoPartial[TwoItemVector[String]]
        .withFieldConst(_.everyItem, "c") // we can provide overrides using .everyItem in DSL
        .transform
    )
    // expected output:
    // Value(value = TwoItemVector(head = "c", tail = "c"))
    ```

!!! tip

    Since `TotalOuterTransformer` and `PartialOuterTransformer` seem to be pretty generic, one can ask why not use them
    to handle all conversions beteen all collections?
    
    The problem is: _how would you define the implicits?_ If you wanted to define them between every pair, that's a
    sqaure of all collections that we would have to handle. If we wanted to define some API so that each of them would
    require only 1 implicit - that's precisly what `TotallyBuildIterable` and `PartiallyBuildIterable` provide.
    
    As a result, defining an Outer Transformer for collection is necessary only when it's a collection with a smart
    constructor and we have to handle a case when we know that this smart constructor would not fail.
    
!!! tip

    Outer Transformers are useful not only for special cases in collections, but they can also be used when one would
    like to handle the conversion inside some wrapper types (that cannot be handled by Chimney OOTB) like e.g. some
    new types implementations. 

### Custom error types

Chimney's derivation supports only 1 error type: `partial.Result[A]`. It allows to effectively combine errors,
provide paths to failed values and chose between fail-fast and error accumulating mode in runtime.

However, projects might use different ones: `Either[String, A]`, `Try[A]`, `ValidatedNel[String, A]`, ... so we might
want to be able to convert back and forth between `partial.Result` and the project's error type.

Conversion into `partial.Result` is handled with `io.scalaland.chimney.partial.syntax._`, which provides `.asResult`
extension methods, while conversion from is a method `.to[ErrorTypeName]`:

!!! example

    ```scala    
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}
    
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial.syntax._
    import scala.util.Try

    case class Foo(str: String, meta: String)
    case class Bar(int: Int, meta: String)

    pprint.pprintln(
      Foo("10", "value")
        .intoPartial[Bar]
        .withFieldComputedPartial(_.int, foo => Try(foo.str.toInt).asResult) // Try -> partial.Result
        .transform
        .asOption // partial.Result -> Option
    )
    // expected output:
    // Bar(int = 10, meta = "value")
    ```

Out of the box, Chimney provides `partial.Result[A]` conversions:

  * from/to `Option[A]`:
    * `(option: Option[A]).asResult: partial.Result[A]`
    * `(option: Option[A]).toPartialResult: partial.Result[A]` (old syntax)
    * `(option: Option[A]).toPartialResultOrString(ifEmpty: String): partial.Result[A]` (old syntax)
    * `(result: partial.Result[A]).asOption: Option[A]`
* from `Either[String, A]`:
    * `(either: Either[String, A]).asResult: partial.Result[A]`
    * `(either: Either[String, A]).toPartialResult: partial.Result[A]` (old syntax)
 * from `Either[partial.Result.Errors, A]`:
    * `(either: Either[partial.Result.Errors, A]).asResult: partial.Result[A]`
    * `(result: partial.Result[A]).asEither: Either[partial.Result.Errors, A]`
 * from `Try[A]`:
    * `(ttry: Try[A]).asResult`
    * `(ttry: Try[A]).toPartialResult` (old syntax)

To enable `.asResult` syntax, all you need to do is providing an `implicit` instance of
`io.scalaland.chimney.partial.AsResult` type class:

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class MyErrorType[A](value: Either[List[String], A])

    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial
    import io.scalaland.chimney.partial.syntax._

    implicit val myErrorTypeAsResult: partial.AsResult[MyErrorType] =
      new partial.AsResult[MyErrorType] {

        def asResult[A](myResult: MyErrorType[A]): partial.Result[A] = myResult.value match {
          case Right(value)        => partial.Result.fromValue(value)
          case Left(head :: tails) => partial.Result.fromErrorStrings(head, tails: _*)
          case Left(Nil)           => partial.Result.fromEmpty
        }
      }

    pprint.pprintln(
      MyErrorType(Right("value")).asResult
    )
    // expected output:
    // Value(value = "value")
    ```

However, since conversion from `partial.Result` needs a dedicated name (`.asErrorTypeName` by convention) it
requires a normal extension methods provided by the user.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    //> using dep com.lihaoyi::pprint::{{ libraries.pprint }}

    case class MyErrorType[A](value: Either[List[String], A])

    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.partial

    implicit class MyErrorTypeResultOps[A](private val result: partial.Result[A]) extends AnyVal {

      def asMyErrorType: MyErrorType[A] = result.asEitherErrorPathMessageStrings match {
        case Right(value) => MyErrorType(Right(value))
        case Left(errors) => MyErrorType(Left(errors.map { case (path, msg) => msg }.toList))
      }
    }

    pprint.pprintln(
      partial.Result.fromValue("value").asMyErrorType
    )
    // expected output:
    // MyErrorType(value = Right(value = "value"))
    ```

### Third-party integrations

Some libraries already provided support for Chimney, and you don't have to provide it yourself:

#### Enumz

[Enumz](https://github.com/scalalandio/enumz) is Scala 2/3 library which creates `Enum[E]` type class, allowing working
with enumeration types (`sealed trait`s, Scala 3 `enum`s, Java `enum`s, `scala.Enumeration`) in a uniform way.
It provides an integration which might be useful if one needs to convert to/from `scala.Enumeration`.

You can find it on [GitHub](https://github.com/scalalandio/enumz) or
[Scaladex](https://index.scala-lang.org/scalalandio/enumz/artifacts/enumz-chimney).

#### Neotype

[Neotype](https://github.com/kitlangton/neotype) is Scala 3 only library which makes working with `opaque type`s
easier. It's similar to other libraries described in
[Libraries with smart constructors](#libraries-with-smart-constructors).

You can find it on [GitHub](https://github.com/kitlangton/neotype) or
[Scaladex](https://index.scala-lang.org/kitlangton/neotype/artifacts/neotype).

#### Refined4s

[Refined4s](https://refined4s.kevinly.dev/) is Scala 3 only library which makes working with `opaque type`s
easier just like Neotype or other libraries described in
[Libraries with smart constructors](#libraries-with-smart-constructors).

You can find it on [GitHub](https://github.com/kevin-lee/refined4s) or
[Scaladex](https://index.scala-lang.org/kevin-lee/refined4s/artifacts/refined4s-cats).

#### Utils (ZIO Prelude integration)

[Utils](https://github.com/kinoplan/utils) is a set of Scala libraries providing, among others, integrations.
One of them is an integration between [ZIO Prelude](https://github.com/zio/zio-prelude) and Chimney, working
similar to [Cats](#cats-integration) and using [Integrations API](#integrations).

You can find it on [GitHub](https://github.com/kinoplan/utils) or
[Scaladex](https://index.scala-lang.org/kinoplan/utils/artifacts/utils-chimney-zio-prelude).

## Reusing Chimney macros in your own macro library

Some parts of the Chimney macros could be useful to developers of other libraries. As part of the 0.8.0 refactor,
we developed:

 - a platform-agnostic way of defining macro logic - see [Under the Hood](under-the-hood.md) for more information
 - `chimney-macro-commons` - the module extracting non-Chimney-specific macro utilities: extracting fields/nullary
   `def`s from classes, extracting constructors and all setters (if available), extracting enum subtypes/values,
   exposing `blackbox.Context`/`Quotes` utilities in a platform-agnostic way, etc
 - an automatic derivation without the standard automatic derivation overhead
 - a recursive derivation engine based on the chain-of-responsibility pattern

For now there aren't many people interested in them, so comments and Chimney-code-as-examples is the only documentation
available.

### `chimney-macro-commons`

This module contains no dependencies on Chimney runtime types, not Chimney-specific macro logic. It could be used to
reuse Chimney utilities for e.g.:

 - extracting `val`ues and nullary `def`s from any class
 - extracting public constructors and setters
 - converting between singleton `Type[A]` and `Expr[A]`
 - providing a platform-agnostic utilities for some common types and expressions

!!! note

    This module is checked by MiMa, its API should be considered stable.

#### macro-commons architecture

The idea behind macro commond, (and whole Chimney), is to avoid using low-level macro API, and coding against higher level
interface, where actual Scala 2/Scala 3 macros are mixed in later (it's a cake pattern):

!!! example

    DSL is defined using path-dependent types, we are using abstract type, extension methods and "companion objects"
    to define our API

    ```scala
    // APIs related to types reporesentation
    trait Types {

      type Type[A]
      val Type: TypeModule
      trait TypeModule {: Type.type =>
        
        def apply[A](implicit A: Type[A]): Type[A] = A

        def isSubtypeOf[A: Type, B: Type]: Boolean
      }

      implicit class TypeOps[A](private val A: Type[A]) {

        def <:<[B](B: Type[B]): Boolean = Type.isSubtypeOf(A, B)
      }
    }
    // APIs related to expressions
    trait Exprs {

      type Expr[A]
      val Expr: ExprModule
      trait ExprModule { Expr.type =>
        
        def asInstanceOfExpr[A: Type, B: Type](expr: Expr[A]): Expr[B]
        def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B]
      }

      implicit class ExprOps[A: Type](private val expr: Expr[A]) {

        def asInstanceOfExpr[B: Type]: Expr[B] = Expr.asInstanceOfExpr[A, B](expr)
        def upcast[B: Type]: Expr[B] = Expr.upcast[A, B](expr)
      }
    }
    // APIs related to e.g. reporting compilation errors
    trait Results {

      def reportError(errors: String): Nothing
    }
    // single trait to mix-in for convenience
    trait Definitions extends Types with Exprs with Reports
    ```

    then we can code against this API:

    ```scala
    trait UpcastIfYouCan { this: Definitions =>

      def upcastIfYouCan[A: Type, B: Type](expr: Expr[A]): Expr[B] =
        if (Type[A] <:< Type[B]) expr.upcast[B] else reportError("Invalid upcasting")
    }
    ```

    Meanwhile, all Scala 2/Scala 3 specific code can be contained inside platform-specific traits that would be mixed-in when composing whole macro:

    ```scala
    // Scala 2
    trait TypesPlatform extends Types {
      val c: blackbox.Context

      import c.universe._

      type Type[A] = c.WeakTypeTag[A]
      object Type extends TypeModule {

        def isSubtypeOf[A: Type, B: Type]: Boolean = Type[A].tpe <:< Type[B].tpe
      }
    }
    trait ExprsPlatform extends Exprs { this: TypesPlatform =>
      import c.universe._

      type Expr[A] = c.Expr[A]
      object Expr extends ExprModule {

        def asInstanceOfExpr[A: Type, B: Type](expr: Expr[A]): Expr[B] = c.Expr[B](q"$expr.asInstanceOf[${Type[B]}]")
        def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = c.Expr[B](q"($expr : ${Type[B]})")
      }
    }
    trait ReportsPlatform extends Reports {
      import c.universe._

      def reportError(errors: String): Nothing = c.abort(c.enclosingPosition, errors)
    }
    trait DefinitionsPlatform extends Definitions with TypesPlatform with ExprsPlatform with ReportsPlatform
    ```

    ```scala
    // Scala 3
    abstract class TypesPlatform(using q: Quotes) extends Types {
      import q.*, q.reflect.*

      type Type[A] = quoted.Type[A]
      object Type extends TypeModule {

        def isSubtypeOf[A: Type, B: Type]: Boolean = TypeRepr.of(using A) <:< TypeRepr.of(using B)
      }
    }
    trait ExprsPlatform extends Exprs { this: TypesPlatform =>
      import q.*, q.reflect.*

      type Expr[A] = quoted.Expr[A]
      object Expr extends ExprModule {

        def asInstanceOfExpr[A: Type, B: Type](expr: Expr[A]): Expr[B] = '{ ${ expr }.asInstanceOf[B] }
        def upcast[A: Type, B: Type](expr: Expr[A]): Expr[B] = expr.asInstanceOf[Expr[B]]
      }
    }
    trait ReportsPlatform extends Reports { this: TypesPlatform =>
      import q.*, q.reflect.*

      def reportError(errors: String): Nothing = report.errorAndAbort(errors, Position.ofMacroExpansion)
    }
    abstract class DefinitionsPlatform(q: Quotes) extends Definitions with TypesPlatform with ExprsPlatform with ExprPromisesPlatform with ResultsPlatform
    ```

    Having both Scala-macro-agnostic logic and platform-specific implementation, we can build compose ourselves the final macro:

    ```scala
    // Scala 2

    // So called macro bundle - class with a single argument - Context - whose method will be called during expansion 
    final class UpcastingMacros(val c: blackbox.Context) extends DefinitionsPlatform(q) with UpcastIfYouCan {

      import c.universe.*

      // we have to align argument names and arity between macro and its definition
      def upcastImpl[A: c.WeakTypeTag, B: c.WeakTypeTag](value: c.Expr[A]): c.Expr[B] = upcastIfYouCan[A, B](value)
    }
    object Upcasting {

      def upcast[A, B](value: A): B = macro UpcastingMacros.upcastImpl[A, B]
    }
    ```

    ```scala
    // Scala 3

    // Putting everything in one class is very convenient...
    final class UpcastingMacros(q: Quotes) extends DefinitionsPlatform(q) with UpcastIfYouCan
    // ...but Scala 3 requires us to store macros inside top-level objects
    object UpcastingMacros {

      def upcastImpl[A: Type, B: Type](a: Expr[A])(using q: Quotes): Expr[B] =
        new UpcastingMacros(q).upcastIfYouCan[A, B](a)
    }
    
    object Upcasting {

      inline def upcast[A, B](inline value: A): B = ${ UpcastingMacros.upcast[A, B](${ value }) }
    }
    ```

The whole premise of this approach relies on a few assumptions:

 * macros will grow bigger and more comples in time
 * library might target more than 1 scala macro system (2.12, 2.13, 3)
 * library authors see the value in separation of concerns, avoiding mixing levels of abstraction, DRY
 * library authors see the valud in coding against higher-level API which encapsulates how some corner cases are handled

For smaller/simpler/short-living libraries it might feel over-engineered.

#### Components of `chimney-macro-commons`

 - [Types](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/Types.scala) - for types and definitions related to type manipulations and build-in `Type` support, e.g.:
    - summoning with `Type[A]`
    - printing type with `Type.prettyPrint[A]`
    - comparison with `Type[A] =:= Type[B]`, `Type[A] <:< Type[B]`
    - creating (`apply`) or matching (`unapply`) some build-in types: primitives, `Option`s, `Either`s, `Iterable`s, `Map`s, `Factory`ies
    - implicit instances for some common types (`import Type.Implicits._`) - required in macro-agnostic code since it is not synthesising
      `c.WeakTypeTag`s nor `scala.quoted.Type`
 - [Exprs](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/Exprs.scala) - for types and definitions related to expression manipulations and build-in `Expr` support, e.g.:
    - creating primitives' literals
    - printing with `Expr.prettyPrint(expr)`
    - creating instances of `Function1`/`Function2` out of `Expr[A] => Expr[B]`/``(Expr[A], Expr[B]) => Expr[C]`
    - creating instances of `Array`s, `Option`s, `Either`s, `Iterable`s, `Map`s
    - suppressing warnings
    - summoning implicits
    - creating `if`-`else` branches and blocks
    - upcasting
 - [Results](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/Results.scala) - for types and definitions related to returning info/error messages from macros:
    - reporting `info` message that compiler should show in output/IDE
    - reporting `error` message that compiler should show as the reason for macro failure
 - [Existentials](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/Existentials.scala) - for types and definitions related to working with unknown types ("existential types"), e.g.:
    - `ExistntialType` or `??` - usable via `import existentialType.Underlying as NewTypeName`
    - `ExistentialExpr` - usable via `import existentialExpr.{Underlying as NewTypeName, value as expr}`
 - [ExprPromises](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/ExprPromises.scala) - for types and definitions related to computing `val`s/`lazy val`s/`def`s/`var`s before knowing the returned `Expr`'s `Type`, caching value as val, caching  derivation as `def`
 - [Definitions](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/Definitions.scala) - for types and definitions related to reading macro configurations:
    - `Definitions` contains all of the above traits for convenience
    - additionally, exposes the content of `-Xmacro-setting` scalac option
 - [ProductTypes](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/datatypes/ProductTypes.scala) - for types and definitions related to extractors and constructors of a product type:
    - `Type[A] match { case Product.Extraction(getters) => ... }` - provides getters (`val`s, `var`s, Java Bean getters, nullary `defs`) - always available
    - `Type[A] match { case Product.Constructor(getters, constructor) => ... }` - provides a constructor - primary constructor if it's public OR
      the only public constructor if there is exactly one
    - `Type[A] match { case Product(getters, constructor) => ... }` - provides both getters and constructor
 - [SealedHierarchies](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/datatypes/SealedHierarchies.scala) - for types and definitions related to finding all subtypes of `sealed trait`s/`sealed abstrcto class`es/Scala 3 `enum`s/Java `enum`s:
    - `Type[A] match { case SealedHierarchy(elements) => }` - provides a list of subtypes of a `sealed` hierarchy/Java `enum`/Scala 3 `enum`
 - [ValueClasses](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/datatypes/ValueClasses.scala) - for types and definitions related to `AnyVal`s and "wrapper"s:
    - `Type[A] match { case ValueClassType(valueType) => ... }` - provides `wrap` and `unwrap` method if `Type[A]` is a subtype of `AnyVal` with unary public constructor
      and public value
    - `Type[A] match { case WrapperClassType(valueType) => ... }` - provides `wrap` and `unwrap` method if `Type[A]` has unary public constructor and public value
 - [SingletonTypes](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/datatypes/SingletonTypes.scala) - for types and definitions related to singleton types:
    - `Type[A] match { case SingletonType(singleton) => ... }` - provides `Expr[A]` if it's a primitive type literal, `case object`, Scala 3 `enum` parameterless
      `case` or Java `enum` value
 - [IterableOrArrays](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-macro-commons/src/main/scala/io/scalaland/chimney/internal/compiletime/datatypes/IterableOrArrays.scala) - for types and definitions related to unified interface for working with Arrays and Scala collections:
    - `Type[A] match { case IterableOrArray(iOrA) => ... }` - provides `Factory`, `.map`, `.to` and `.interator` methods for Arrays/iterables/maps

#### macro-commons examples

 * Chimney's source code - since 0.8.0 Chimney has been build upon this architecture
 * [`chimney-macro-commons` template](https://github.com/scalalandio/chimney-macro-commons-template) - can be used as a GitHub template

### `chimney-engine`

This module exposes Chimney derivation engine to make it easier to use in one's own macros. It assumes that user would
implement its macro the same way as Chimney does it, and with similar assumptions
(see [Under the Hood](under-the-hood.md)).

The only documentation is
[the example code](https://github.com/scalalandio/chimney/blob/{{ git.short_commit }}/chimney-engine/src/test/) which
illustrates how one would start developing a macro basing on Chimney engine.

!!! warning

    This module exposes Chimney internal macros, their API can change to enable new feature development, so consider it
    unstable and experimental!
 
    The module's version matches `chimney` version it was compiled against, but it should NOT be considered a semantic
    version.
