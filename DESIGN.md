# Design

Before one starts looking around the codebase, there might be a few things that are useful to understand first:

* certain things are computed only by the compiler and don't exist in runtime
* some things have to be instantiated in runtime
* how DSL enable passing information to macros
* what derivation code needs to take into consideration and how it affects the infrastructure

## Runtime vs compile time

The first thing we need to understand is which part of Chimney are performed by compiler and which are executed in
the runtime. Let's look at some examples.

### Configured transformation

Let's take:

```scala
case class Foo(a: Int, b: String)
case class Bar(a: Int, b: String, c: Double)
```

```scala
Foo(1, "test").into[Bar].withFieldConst(_.c, 3.0).transform
```

The simplified version of how the code above works:

1. `.into[Bar]` wraps `Foo(1, "test")` value. The wrapper (here: `TransformerInto`) would store
   **both the transformed value and possible field/coproduct value overrides**
2. these overrides are stored in `RuntimeDataStore` (currently implemented as `Vector[Any]`) -
   `.withFieldConst(_.c, 3.0)` adds `3.0` as a value to this vector.
   **Both wrapping and override appending would happen during runtime**, so this DSL imposes some overhead
3. the final `.transform` would generate a code similar to:
   ```scala
   {
     val transformerInto = ??? // stub for what is actually here
     new Bar(
       transformerInto.source.a,
       transformerInto.source.b,
       transformerInto.td.runtimeDataStore(0).asInstanceOf[Double]
     )
   }
   ```
4. since there might be many overrides in `td.runtimeDataStore(0)` and the macro needs to know which field override is on
   which position, **DSL needs to remember somehow what each index in the vector overrides**. For that purpose there
   exist `TransformerCfg`, a phantom type (a type used only in compile time) which acts as a type-level list where each
   such information could be prepended. (You can think of it as of a tuple, which never get instantiated and only exist
   as expandable list of types). Each time user adds some override code is generated which would append a value in
   runtime, but also modify the type of the wrapper. Then when the macro is called it can read configuration from
   the type and compute which override is stored under which index.

> Types computed by `.withField*`, `.withSealedSubtype*`, `.enable*` and `.disable*` are intended to be inferred, not shown
> to the user and not used by the user manually. For that reason all `*Cfg` and `*Flags` are defined in `internal`
> subpackage.

Very similar thing happen when calling

```scala
Transformer.define[Foo, Bar].withFieldConst(_.c, 3.0).buildTransformer
```

1. main difference is that there is no `Foo(1, "test")` which will be wrapped, so DSL wrapper (here:
   `TransformerDefinition`) passes around only `RuntimeDataStore`
2. similarly to the previous example `.with*` methods put overrides into `RuntimeDataStore` and refine the config type
   by prepending type level information
3. the final result returned by `.buildTransformer` is something similar to:
   ```scala
   {
     val transformerDefinition = ??? // stub for what is actually here
     new Transformer[Foo, Bar] {
       def transform(src: Foo): Bar = new Bar(
         src.a,
         src.b,
         transformerDefinition.runtimeDataStore(0).asInstanceOf[Double]
       )
     }
   }
   ```

### Automatic transformation

When calling

```scala
Bar(1, "test").transformInto[Foo]
```

**no override or flag needs to be stored in wrapper**, and the method itself summons implicit
`Transformer.AutoDerived[Bar, Foo]`. If users didn't provide their own `Transformer[Bar, Foo]` and instance will be
created by calling `Transformer.derive[Bar, Foo]`. This method doesn't require any wrapper for building something which
stores transformed value next to overrides container, so it can generate similar code:

```scala
// a.transformInto(implicit b) internally
// just calls b.transform(a)
Bar(1, "test").transformInto(
  // created by implicit macro:
  new Transformer.AutoDerived[Bar, Foo] {
    def transform(src: Bar): Foo = new Foo(
      src.a,
      src.b
    )
  }
)
```

### Partial transformation

Partial transformers works on the same principles, when it comes to what is represented in type level, what is stored
in runtime, and how DSL is defined. **The true difference lies inside macros being called by the DSL**.

## DSL implementation

Macros make is relatively easy to access the value to which macro is attached. It might be very hard though to obtain
the whole expression which built this value. Especially, if you consider that user could do:

```scala
val expr = Foo(1, "test").into[Bar]
if (condition)
  expr.withFieldConst(_.c, 3.0).transform
else
  expr.withFieldConst(_.c, 4.0).transform
```

That's why it is simpy easier to treat each modifier method as a checkpoint which would store all the added information
in the value's type.

There are actually 2 sets of configuration options that are stored by DSL in type level:

* `TransformerCfg` stores information about field and coproduct overrides, most of them is accompanied by a runtime
  value (either some constant or a function)
* `TransformerFlags` store information about options which aren't tied to a particular field or subtype, so they can be
  considered global - indeed there is a way for sharing these flags by all derivations in the same scope
  (`TransformerConfiguration`).

In Scala 2 overrides are implemented with [whitebox macros](https://docs.scala-lang.org/overviews/macros/blackbox-whitebox.html)
which allow read which field was selected with `_.fieldName` syntax, turning it into a `String` singleton type (e.g.
`"fieldName"` type) and prepending type level information to the type (e.g.
`TransformerCfg.FieldConst["fieldName", TransformerCfg.Empty]` prepends information that 0-index in `RuntimeDataStore`
contains override for `"fieldName"` to an empty config).

In Scala 3 the mechanism is similar except whitebox macros are replaced by
[`transparent inline`](https://docs.scala-lang.org/scala3/guides/macros/inline.html#transparent-inline-methods) macros.

> DSL has a separate macro implementation for Scala 2 and 3 since there was a negligible amount of logic shared between
> them.

Flags, since they don't need to extract any data to generate type information, are just "normal" Scala code which
prepends flags to the flag type representation.

## Derivation implementation

To understand abstractions in derivation macros some assertions need to be clarified:

* code will be cross compiled for 2.12/2.13/3 (and for JVM, Scala.js and Scala Native)
* each bugfix would have to be shared by all codebases
* the logic between all Scala versions needs to stay as similar as possible unless there are some good non-accidental
  reasons to make behavior different
   * the logic is already pretty complex
* DSL should not differ so that Chimney would not be a blocker for migration from one version of Scala to another
* the maintenance of this project is planned for years so one-time solution is off the table

For that reasons maintaining 2 completely distinct implementations would not be sustainable: despite an extensive test
suite there would be a lot of subtle differences in the behavior that the tests didn't catch. Instead, the decision was
made to share as much of Chimney logic between Scala 2 and Scala 3 as possible.

It has several consequences:

* the code of macros uses traits with path-dependent types and abstract methods to design shared logic - it is similar
  to how Endpoints4s or Endless4s libraries are defined (the technique was described e.g. in
  [C. Hofer et al. **Polymorphic Embedding of DSLs**, GPCE, 2008](https://www.informatik.uni-marburg.de/~rendel/hofer08polymorphic.pdf))
  (it is much easier to understand just by looking around e.g. `Types` and `TypesPlatform` and seeing how they are used)
* most common types used are `type Type[A]` and `type Expr[A]`, defined as abstract in shared code, and specified to
  concrete implementation in platform-specific code
* Scala 3 quotes depend on implicit `scala.quoted.Type` _a lot_, so shared code has to pass around types everywhere
* since only a few types can be known upfront and named: source value-type, target-type, types based one them, a lot of
  types are virtually existential: types of each constructor parameter, types returned by getters, subtypes. This
  requires us to express some types as existential types with values of types using these existential types. Often it's
  `Type[something]` or `Expr[something]` (of both at once with the same existential type used), so certain abstractions
  needed to be designed (see `Existentials`)

Additionally, there are several implications of how code is generated:

* partial transformers attempts to avoid/delay boxing, so as many expressions as possible would try to wrap in
  `partial.Result` only when absolutely unavoidable. This means that code derived for some part of partial transformer
  expression doesn't have to be partial - there is a need for something modeling the same idea as
  `Either[Expr[A], Expr[partial.Result[A]]]` - this resulted in `TransformationExpr[A]`
* code is derived recursively, and:
   * source and target type
   * location of overrides
   * source value
   * configurations

  has to be passed around, so some `TransformationContext` of the derivation is useful to pass everything with a single
  value
* macros should not fail fast, but rather aggregate all the errors making derivation impossible, so that error message
  for a single macro could display all known errors at once. Instead of manually combining some
  `Either[List[TransformerError], TransformationExpr[A]]` a dedicated monad comes handy - `DerivationResult` monad.
* this monad can be also used as a `Writer` monad for gathering logs, because both in Scala 2 as well in Scala 3 for
  each macro only the first logging call (for each logging level: info/warn/error) would print, all the following would
  be no-op. With this monad logs could be aggregated in some list and then the final message could be printed at once
  (additionally, such logs can be structured).

> All of the above, are simplified explanations for why certain decisions were made and why certain utilities exists.
> Exact implementation for these utilities will change over time.
