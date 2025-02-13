# Under the Hood

If you think that what Chimney does is magic, it might help you if you learn how exactly expressions are generated.

This section aims to explain some mechanics of Chimney, at least those that might affect users and aren't just
implementation details. It might explain some of the output you'll see if you
[enable logging from macros](troubleshooting.md#debugging-macros).

## Introduction

Chimney's behavior was designed with users' convenience in mind, starting with the best draft of an API and tests and
then figuring out a way to implement such behavior. Over time, with feedback, we had a larger and larger list of
things that users considered to be intuitive (or at least safe, especially for corner cases), which we could use as
guidance, iteratively improving the design.

Oftentimes, we received conflicting opinions about what is intuitive (or safe) and we had to decide ourselves how to
turn them into a deterministic specification. Which is why some of the decisions here will sound arbitrary. Yet they are
deterministic and tested with specs on every supported version of Scala on every platform.

To understand the mechanics of Chimney, we need to discuss which things are calculated within DSL, when calling
fluent API, and which are done within macros. Which things need to be computed in the runtime and which can be computed
completely in the compile time. We will discuss each of these, recalling the reasons behind design decisions.

## DSL

How users call the API and what they expect to happen, enforces a lot of design decisions. Here, we'll analyze what
happens in DSL, when we're triggering automatic and semiautomatic derivation or customizing the transformer with flags
or overrides.

### How DSL summons `Transformer` instance

Users' expectations start with the attempt to transform a value without any customization.

!!! example

    ```scala
    import io.scalaland.chimney.dsl._
    val source: Source = ??? // stub for what is actually here
    source.transformInto[Target]
    ```
    
    The code above should:
    
      - use `Transformer[Source, Target]` if the user provided it
      - automatically provide a transformation if and only if the user didn't provide it
      - generate a readable error message if it cannot be generated (_implicit not found_ doesn't make the cut)

It sounds like `transformInto` should be a macro, but someone might also want to use it similarly to other
libraries with (automatic) derivation.

!!! example

    Passing `Transformer` with short lambda syntax

    ```scala
    val customTransformers: List[Transformer[Source, Target]] = ??? // stub for what is actually here
    val source: Source = ??? // stub for what is actually here
    list.map(source.transformInto(_))
    ```

    Passing `Transformer` manually

    ```scala
    val transformer: Transformer[Source, Target] = ??? // stub for what is actually here
    val source: Source = ??? // stub for what is actually here
    source.pipe(_.transformInto(transformer)).pipe(println)
    ```

These aren't common cases, but for debugging reasons sometimes you may want to a pass value explicitly. You wouldn't
do that with a macro that summons implicit internally. 

So, should the extension method look like this?

!!! example

    ```scala
    // extension method on [From](from: From)
    def transformInto[To](implicit transformer: Transformer[From, To]): To = transformer.transform(from)
    ```

Well, to make it work with automatic derivation, we would have to put `Transformer[From, To]` into implicit scope.
We'll get to details later, but for now: in macros we  **never** want to summon implicits that
**the user did not provide**, we **always** want to be able to summon implicit that **the user did provide**.
If users are supposed to provide their instances with `implicit` `Transformer`s and automatic derivation would also
provide an `implicit` `Transformer`, how we would distinct user's instances from automatic ones? The solution is to make
them slightly different types:

!!! example

    ```scala
    // Transformer is a subtype of AutoDerived (is more specific/constrained)
    trait Transformer[From, To] extends Transformer.AutoDerived {
      def transform(src: From): To
    }
    object Transformer {
      trait AutoDerived[From, To] {
        def transform(src: From): To
      }
      // Somewhere in the companion object we would define an implicit derivation of AutoDerived,
      // making it a low-priority(!!!) implicit and a macro.
    }
    ```
    
    ```scala
    // extension method on [From](from: From)
    def transformInto[To](implicit transformer: Transformer.AutoDerived[From, To]): To = transformer.transform(from)
    ``` 

With such setup:

  - the user's `Transformer` would always be attempted first
  - then summoning would fall back on `Transformer.AutoDerived` if there was none (being low-priority) 
  - since the automatic derivation would be a macro, it could produce a nice compiler error instead of
    _implicit not found_ (in macros there are ways to fail compilation with an error)
  - when needed, we could rule out automatic derivation just by summoning `Transformer` instead of
    `Transformer.AutoDerived` (which is done both in macros and in
    [`import io.scalaland.chimney.syntax._`](cookbook.md#automatic-semiautomatic-and-inlined-derivation))

### How DSL summons `PartialTransformer` instance

`PartialTransformer`s are virtually the same as `Transformer`s. The only difference is that you should always be able to
perform a partial transformation when there is `Transformer`, so the priority order of implicit summoning looks like
this:

  - the compiler attempts to find an implicit `PartialTransformer` provided by the user
  - then attempts to find an implicit `Transformer` provided by the user and lift it to a `PartialTransformer`
  - finally attempts automatic derivation with a macro

### How DSL manages customizations

The API call to customize the derivation could look like this:

!!! example

    ```scala
    val source: Source = ??? // stub for what is actually here
    source.into[Target].withFieldConst(_.a, value).enableMethodAccessors.transform
    ```

It would have to:

  - ignore the existing `Transformer[From, To]` if there is one (if you wanted the existing one, you wouldn't customize it)
  - make sure that `value` is computed exactly once (in API we are passing computed value to a method, if it
    suddenly becomes an AST tree which we would copy-pasting around it could change the behavior of a program)
  - make sure that value is used only for the `a` field of `Target`
  - and that derivation would not forget that Method Accessors flag was turned on
  - somehow `source` would have to be saved - we cannot lose it in between all these chained calls 

#### Carrying around the runtime configuration

How to achieve it with the summoning of an implicit? Not easily. This implicit summoning would have to somehow carry
all the necessary information to create a `Transformer` in the type and also carry around `source` and `value`.

It is much easier to just skip the part where you create a `Transformer`, instantiate it, and pass into it all the
transformed values. The final `.transform` generates the transformation expression directly without an intermediate
type class instantiation.

!!! note

    If now you are thinking _"hey, does it mean that they are **NOT** doing this?"_:
    
    ```scala
    // created by .transform macro
    val transformer = new Transformer { /* customized body */ }
    transformer.transform(source)
    ```
    
    then you are right. We do not have to unwrap the value by calling anything.
    
    Actually, when you call macros, the whole engine generates the expression of type `To` (or `partial.Result[To]`).
    `source.into[Target].transform` inlines this generated expression directly, while automatic derivation,
    `Transformer.derive[From, To]` and `Transformer.define[From, To].buildTransformer` additionally:
    
    ```scala
    new Transformer[From, To] {
      def transform(src: From): To =
        // use `src` as source expression for derivation (Expr[From])
        ${ derivedToExpression: Expr[To] } // paste the derived expression as Transformer's `transform` body
    }
    ```
    
    So only the methods that demand a type class would wrap the derived expression with it. Internally, almost all the time
    Chimney build just a `To` expression which attempt to compute the value in the leanest way possible.

So the next question is: how to pass to it all runtime values and configurations collected on the way from `.into` till
`.transform`?

Well. It appears that runtime values are the easiest to store in runtime. When we call `.into` we are creating a wrapper

!!! example

    ```scala
    class TransformerInto[From, To]( /* ... */ ) {
      // ...
    }
    // extension method on [From](source: From)
    def into[To]: TransformerInto[From, To] = new TransformerInto[From, To]( /* ... */ )
    ```

    (`PartialTransformer` has its own `PartialTransformerInto`).

This is unavoidable, after all we have to define all these `.withField*`, `.withSealedSubtype*`, `.enable*`, `.disable*`
and `.transform` define on something. So this something happens to be a `TransformerInto` class - it exists at runtime, so
it is a great candidate to pass around `From` value for future consumption. But `From` is a fixed type. What about
all these constants and functions that we also need to carry around? Well, inside `TransformerInto` there is also
another value, let's call it `RuntimeDataStore` which collects all these values as we chain methods. Upcasting them
to a common supertype: `Any`. So from the JVM perspective, after type erasure, what we have is similar to:

!!! example

    ```scala
    new TransformerInto(source)
      .methodAppendingValue(constant: Any)
      .methodAppendingValue(function: Any)
    // ...
    ```

At the end of this chain is an object which the macro (from `.transform`) can access to extract from it a `From` value
and a `RuntimeDataStore` value (currently `type RuntimeDataStore = Vector[Any]`). It means, that as long as macro would
know what is stored at each `RuntimeDataStore` index, and what is the runtime type of the value, it would have all the
information we provided it.

#### Carrying around the type-level configuration

The easiest way to store this kind of information was by introducing a phantom type (a type used only during
compilation) which represented configuration as a sort of list: you start with a type representing an empty config,
and then you prepend each config similar to how cons works in normal list. You end up with something like:

!!! example

    ```scala
    source.withFieldConst(_.a, ???).withFieldRenamed(_.b, _.c).transform
    // has a Cfg type like
    TransformerCfg.FieldRenamed[fieldBType, fieldCType, TransformerCfg.FieldConst[fieldAType, TransformerCfg.Empty]]
    // Let's not dive into how field names are represented.
    ```

This type is built by whitebox macros (on Scala 2) or transparent inline def (Scala 3) - macros would read the tree of
the `_.fieldName`, extract `fieldName` out of it, turn it into a type, and put it inside the new config type.
(These macros would also append the constant/function to `RuntimeDataStore`).

The macro can parse this type, recovering the order in which types were applied, the name of each field, the
type of each enum's subtype, etc. From that it can calculate on which index each **override** is stored.

Not everything though has to be stored in runtime. If you take a look at the source code you'll notice that there is
another type, using similar tricks to `TransformerCfg`, called `TransformerFlags`. It is used to carry around settings
(in the form of **flags**), that have no relation to a particular type, but still drive the macro logic. Contrary to
`TransformerCfg` flags can also be shared: you can create an implicit `TransformerConfiguration` - this is the only
implicit always looked for by:

  - automatic derivation
  - semiautomatic derivation
  - inlined derivation (`into.transform`)

And since it is an implicit, it can be shared between several different macro expansions.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._

    // All transformations derived in this scope will see these new flags (Scala 2-only syntax, see cookbook for Scala 3!).
    implicit val cfg = TransformerConfiguration.default.enableMacrosLogging

    "test".transformInto[Option[String]]
    "test".into[Option[String]].transform
    ```

`PartialTransformer`s are virtually identical to `Transformers` when it comes to this mechanics. They have a few more
configs, meaning that they need a separate builder, but they use the same `TransformerCfg` and `TransformerFlags`. 

### How DSL allows semiautomatic derivation

`Transformer.derive[From, To]` works the same way as the automatic derivation - a macro is called, and internally it
generates the expression of type `To`. Before returning the expression the macro wraps it with a type class. The only
differences between automatic and semiautomatic is `implicit` keyword and upcasting `Transformer` to
`Transformer.AutoDerived`. 

`Transformer.define[From, To].buildTransformer` works like a mix of `Transformer.derive[From, To]` and
`from.into[To].transform`: it carries around `RuntimeDataStore` like `into.transform`, but don't need to store
`source: From` (it will be provided to the type class via argument). Then it derives the expression of type `To`
and wraps it with a type class. 

### Scala 2 vs Scala 3 in DSL

Definitions of data that have to be accessed in runtime are shared between Scala 2 and Scala 3. Differences requiring
separate implementations are mostly about:

  - extension methods (`implicit class` + `AnyVal` for Scala 2, `extension` for Scala 3) - distinct implementations
  - calling macros (`def` `macro` for Scala 2, `inline def` and `transparent inline def` for Scala 3) - either
    distinct implementations or some `trait ClassNamePlatform` mixins

However, both Scala 2 and Scala 3 use `implicit`s, Scala 3 doesn't use `given`s since they could introduce a difference
in the behavior (e.g. Scala 2 could still `import module._` while Scala 3 would require `import module{*, given}` -
in general such separation is a good idea, but not when it comes to cross-compilation). 

## Derivation

Once DSL gathers all the user's inputs and requirements (in the form of: input value, source and target types, flags and
overrides), it can pass this information to macros and let them compute the final expression (or error message). 

### Initializing macro

In the beginning, macros would perform several tasks that unblock all the future work:

  - if we are deriving inlined expression, macro will need `Expr[From]` (if this is `PartialTransformer` it will **also**
    require `Expr[Boolean]` representing fail fast flag)
  - if we are deriving type class, macro would need to create a type class body (sort of), which would create `Expr[From]`
    coming from `Transformer`'s parameter
  - if we are using overrides, macro needs to figure out `Expr[RuntimeDataStore]`
  - finally macro needs to parse type-level representations of config and flags

All of these data would be put into a single object which would be passed around (`TransformationContext`) - macros do
not use globals since every time macros need to do a recursive derivation we are creating a modified copy of this object.

### Total vs Partial

If we only worked with `Transformer`s and `PartialTransformer`s were not a thing, we would always assume that the computed
expression is `Expr[To]`. If we only derived `PartialTransformer`s then... we could end up with either `Expr[To]` or
`Expr[partial.Result[To]]`. With Partial derivation we do not need to always lift values from `A` to `partial.Result[A]`
as soon as we get it - by delaying the wrapping as long as possible we are avoiding allocations.

!!! example

    ```scala
    //> using dep io.scalaland::chimney::{{ chimney_version() }}
    import io.scalaland.chimney.dsl._
    import io.scalaland.chimney.Transformer

    case class Foo(a: Int, b: Int, c: Int)
    case class Bar(a: String, b: String, c: String)

    implicit val int2string: Transformer[Int, String] = _.toString

    Foo(1, 2, 3).intoPartial[Bar].transform
    ```
    
    would NOT generate anything similar to
    
    ```scala
    val foo = Foo(1, 2, 3)
    for {
      a <- partial.Result.fromValue(int2string.transform(foo.a))
      b <- partial.Result.fromValue(int2string.transform(foo.b))
      c <- partial.Result.fromValue(int2string.transform(foo.c))
    } new Bar(a, b, c)
    ```
    
    but rather:
    
    ```scala
    val foo = Foo(1, 2, 3)
    partial.Result.fromValue(
      new Bar(
        int2string.transform(foo.a),
        int2string.transform(foo.b),
        int2string.transform(foo.c)
      )
    )
    ```

This is a very important property, allowing us to avoid many allocations, but it also means that hardly ever we can
**assume** inside a macro what is the type of the computed expression. Most of the time it's something like a
`Either[Expr[To], Expr[partial.Result[To]]]` (let's call it `TransformationExpr[To]`.), where we will compute some
transformation and then check if it's Total or Partial expression to decide what to do with it. 

It might happen that after a whole derivation for Partial the `TransformationExpr[To]` is still Total. In such case
it will be wrapped with `partial.Result` as a last step, creating only 1 wrapping in the whole expression. 

### Derivation rules

Once we create the `TransformationContext[From, To]` we have all data in one place to start doing the actual
`TransformationExpr[To]` derivation. We can define a set of `Rule`s stating
_if this and this conditions are met, this rule applies_ (the conditions can be about `From`/`To` types, whether
the derivation was triggered by Total or Partial, or a presence of a particular config). If a `Rule` would be matched,
it would attempt to derive the expression. This attempt can either succeed or fail. The `Rule`'s failure fails also
the whole derivation.

But the `Rule` might decide that this particular case should not be handled and pass. This is not considered an error 
on its own. When a `Rule` does not apply another `Rule` is tested, and then another, until one of them decides to handle
the case. Only after all `Rule`s decide to pass we are failing compilation with an error telling the user that it is not 
a case handled by Chimney.

How Chimney group/split different `Rule`s use cases into separate values is an implementation detail that could
evolve as new use cases (and bugs) are discovered. Their order is also separately (and deterministically!) defined for
each platform (perhaps some cases would only appear on a single Scala version or a single platform?). Nonetheless,
there are some intuitions about the order in which `Rule`s are tried, which should not change:

  - the first `Rule` to attempt is checking if the user provided us with an `implicit` and if there are any reasons to
    ignore it
  - the second `rule` to attempt is whether the value has to be converted at all, perhaps it is already at the right type
    or could be with just upcasting
  - then we have a whole much of special cases for: value classes, `Option`s, `Either`s, `Array`s, collections... which
    could handle more edge cases than a generic rule for product types or sealed hierarchies. Their order is defined
    empirically - you throw a lot of tests and see if there is some Feature Interaction Bug coming from the current
    order of `Rule`s, which could be fixed with adding some guard to one of them or by changing their order (because one
    is more specific than the other, and so the generic one would catch all the cases that we would prefer to be handled
    by a special casing one)
  - finally, we have 2 most generic rules: one rewriting classes into classes with public constructors (that includes:
    `case class`es, Java Beans, Plain Old Java Objects, and much more!) and one rewriting sealed hierarchies (sealed
    either by Scala or Java, so that includes `sealed trait`s, `sealed abstract class`es, Scala 3's `enum`s, and Java's
    `enum`s)

A bit more information about each of these `Rule`s (without getting into details that could change over time) is
presented below.

#### Summoning implicits

Before attempting to summon any `implicit`, the `Rule` checks if it should do it in the first place:

  - if we provided overrides then the conversion for which we provided it should not use `implicit` (because how then
    we would use them?)
  - semiautomatic derivation has a guard against summoning implicit for itself. It prevents issues like

    !!! example
  
        ```scala
        implicit val transformerFromTo: Transformer[From, To] =
            Transformer.derive[From, To] // implicitly[Transformer[From, To]] == transformerFromTo - cyclic dependency
        ```

    This guard is removed when derivation enters into a recursive mapping of fields/subtypes/inner elements because
    there the recursion is wanted.

If there are no reasons to pass, `Rule` attempts summoning.

For `Transformer`s it could only summon `Transformer` instance, so if it finds it, we are done.

For `PartialTransformer`s we have 4 possible cases:

  - there is only `PartialTransformer[From, To]` - then we are using it and create `partial.Result` value
  - there is only `Transformer[From, To]` - then we will use it, and it will help us avoid wrapping with `partial.Result`
    a little bit longer
  - there are both `PartialTransformer[From, To]` and `Transformer[From, To]`, and no configuration telling us which to
    pick - this is considered ambiguous and fails the derivation
  - there are both `PartialTransformer[From, To]` and `Transformer[From, To]`, configuration telling us which to pick

This rule is special in that if no `implicit` is found, then `Rule` recovers to "passing on " rather than "failing". 

#### Upcasting

Before attempting to upcast, the `Rule` checks if it should do it in the first place:

  - if we provided overrides then the conversion for which we provided it should not upcast (because how then we would
    use them?)

If `From` is a subtype of `To` and there are no reasons to pass, the rule upcasts `From` expression and returns success.

#### Special cases

Chimney would have specialized expression generation for things like:

  - singletons types - we might check if the target is a singleton type (`case object`, Scala 3 enum's paremeterless
    `case`, literal-based singleton type) and return the only possible value
  - `AnyVal`s - we might check if wrapping/unwrapping/rewrapping is needed, and trigger a recursive derivation for
    the wrapped types
  - `Option`s - we might check if wrapping/unwrapping (only in Partials)/mapping is needed and trigger a recursive
    derivation for the inner types
  - `Option`s - we might check if wrapping/rewrapping is needed and trigger a recursive derivation for the inner types
  - `Array`s and collections - since the type of "outer" type (collection type/array) can change, we not only need to
    map elements but also potentially covert the collection. In the case of `PartialTransformer`s we also need to traverse
    these collections to accumulate errors and add indices/keys to the error information

For each of these `Rule` might decide whether the case should be handled. Some of these rules needs to be tested before
some other rules (e.g. there might be several `Rule`s for `AnyVal`s with a specific order) and have no requirements about
other rules (e.g. `Rule`s handling `Option`s might be unrelated to `Rule`s handling `AnyVal`s and not affect each other).  

#### Product types

A Product Rule is an umbrella name for transformations that initially only handled product types (`case class`es and
`case class`-like), but currently handle every case when we extract fields or methods, transform them, and put the
results in the constructor (and maybe also setters).

This rule is very generic which is why is tested as last-but-one. To qualify for this rule the target type needs to have
a public primary constructor and be non-abstract. If it is, the rule is applied and failure to successfully map
fields and methods of the source type into the constructor's arguments and setters of the target type fails derivation.

The derivation has a few stages:

  - lists of possible getters (the source type) and the possible arguments (constructor + setters, the target type)
    are made
  - manual overrides are applied
  - arguments/setters which didn't have a manual override are being resolved by macro: if either type is s tuple,
    matching is done by the position, if neither is, matching is done by name
    - during matching, we're checking if inherited definitions should be used, if methods are allowed, if Bean
      getters/setters are allowed, etc
    - for each matching we are triggering a recursive derivation of an expression (from the source field type into
      the target argument type)
  - as the last resort, we are checking if fields with no override nor match can use fallback values  
  - if no derivation failed, we can combine expressions for each argument into an expression building a whole
    constructor call
    - if the transformation is Total (or all expressions are Total), we can just pass them to the constructor and
      return the computed Total expression
    - if at least one expression is Partial, the whole returned expression also needs to be Partial, but its
      construction depends on how many Partial expressions we need to combine:
      - 1 Partial argument - can be `map`ped
      - 2 Partial arguments - can use `partial.Result.map2` or similar
      - 3 or more Partial arguments - to be fast and avoid unnecessary allocations we need to build ourselves
        and expression which would respect `failFast` flag and avoid allocation of intermediate results using e.g.
        `null`s and mutability (this is safe since the user cannot observe that nor break it)

This is the most complex `Rule` that Chimney has to consider.

#### Sealed hierarchies

The last rule is the transformation of `sealed` hierarchies (and/or Java's `enum`s). This rule applies when both
types are Scala's `sealed` (Scala's `enum` including) or Java's `enum`s.

This rule is also very generic, although not as generic as the Products Rule, which is why it has to be among the last
ones. It is tested after Product Rules to allow cases when the `sealed` type is not `abstract` and can be instantiated.
To succeed this `Rule` has to pattern-match in the source type, and for each of its subtypes (exhaustive pattern-matching)
find a mapping.

The derivation has a few stages:
 
  - lists of subtypes of both the source type and the target type are made
  - manual overrides are applied
  - the source type's subtypes that did not have a manual override are being resolved by macro, matching is done by name
  - if no derivation failed, we can combine expressions for each argument into an exhaustive pattern-matching
    - if the transformation is Total (or all expressions are Total), we only need to upcast each result to
      the target type
    - if at least 1 transformation is Partial we need to lift all the other expressions to `partial.Result`

!!! note

    To make sure that there are no "unused" warnings that the user could not fix (we **have to** name the value during
    pattern-matching to start derivation **and then** we learn if it's needed), we have to mark each variable as used.
    
    This is done with:
    
    ```scala
    val _ = value
    ```
    
    syntax. However, if you log from the macro, you might notice that the compiler presents it differently:
    
    ```scala
    (value: ValueType @scala.unchecked) match {
      case _ =>
        ()
    }
    ```
    
    which looks much more disturbing and suggests a performance penalty. However, in the bytecode you can find 1 extra
    `istore_1` (tested on Scala 3), so it is not slow nor unsafe, only ugly.
    
!!! note

    In case there are type parameters in `sealed trait`, you will have a pattern-match (or specifically a type-match in
    our case) which is either:
    
      - not using type parameters and passing `?` instead (potentially making the compiler complain that types do
        not match)
      - using type parameters and making compiler complain that types cannot be checked in the runtime
      
    To prevent both issues we are passing type parameters (to have the correct types of extracted values) and annotating
    the matched value with `@scala.unchecked`.

### Merging transformations and `Patcher`s

Merging transformations require storing the list of possible fallbacks inside the transformation context, then each
rule has to look at it, and decide whether fallbacks apply and if they should be propagated further, e.g.

  - product rule obtains the list of fields for each fallback and tries to use it if the original source has no such field,
    then it uses fallbacks' fields of names matching with target field as the fallbacks for resursive derivation
  - options and eithers rules look at the flags to decide whether or not to merge multiple `Option`s/`Either`s with `orElse`,
    and then they won't propagate fallbacks further
  - similarly collections might be combined tohether with `++` if there is a flag allowing it

In other words, merging has to be supported separately by every rule that should allow it.

`Patcher`s have been rewritten in Chimney 1.7.0 to be a specialized version of merging transformations:

!!! example

    ```scala
    originalValue.patchUsing(patch) // or
    originalValue.using(patch).patch
    ```

    is similar to

    ```scala
    patch.into[PatchedValue].withFallback(originalValue).transform
    ```

but with a few changes, like e.g.:

  - supporting implicit `Patcher` rule
  - failing on unused source value by default
  - `SourceOrElseFallback` strategy for `Option`s and `Either`s
  - dedicated support for updating Option with Option of Option, Either with Option of Either, collection with Option
    of collection
  - specialized errors messages for certain cases

This enabled porting to `Patcher`s many features that `Transformer`s were already supporting without douplicating
the effort, allowed patching to be recursive, etc.

### Error handling and logging

Each macro expansion can emit only 1 message of each of these logging levels: info, warn, error, trace. Each next call
will be treated as a no-op. There are `println`s, but they do not work in: Scastie and build servers (`println`s happen
on a server that only sends you "proper" logs from the compilation).

Meanwhile, Chimney users should receive a list of all aggregated errors that failed the derivation. The only way we
could do it (considering the above) is by gathering all the information to log with something similar to `WriterT` and
collect all the errors with something similar to `EitherT` of some error semigroup. Internally there is a structure
called `DerivationResult` which does exactly that and also stores logs in a structured way. 

Thanks to that, we can both print error information about each tested failed derivation with an exact cause, and
print a structured log of the whole derivation process.

### Scala 2 vs Scala 3 in derivation

As much code as possible (but not more) is shared between Scala 2 and Scala 3. This way we want to avoid 2 separate
codebases that would inevitably diverge through time, with different conventions, trade-offs, etc. Differences in
behavior between 2 and 3 should be justifiable and documented, and never accidental.

The way we achieved this is by using a similar pattern to
[C. Hofer et al. **Polymorphic Embedding of DSLs**, GPCE, 2008](https://www.informatik.uni-marburg.de/~rendel/hofer08polymorphic.pdf)
used in Endpoints4s and Endless4s:

!!! example "Shared codebase"

    ```scala
    trait Definitions extends Types with Exprs
    ```
    
    ```scala
    trait Types {
      type Type[A] // abstract type

      val Type: TypeModule
      trait TypeModule { this: Type.type =>
        // abstract companion object
      }
    }
    ```
    
    ```scala
    trait Exprs { this: Types =>
      type Expr[A] // abstract type

      val Expr: ExprModule
      trait ExprModule { this: Expr.type =>
        // abstract companion object
      }
    }
    ```
 
!!! example "Scala 2-only codebase"

    ```scala
    trait DefinitionsPlatform
        extends Definitions
        with TypesPlatform
        with ExprsPlatform {
      val c: scala.reflect.macros.blackbox.Context
    }
    ```
    
    ```scala
    trait TypesPlatform { this: DefinitionsPlatform =>
      type Type[A] = c.WeakTypeTag[A]

      object Type extends TypeModule {
        // ...
      }
    }
    ```
    
    ```scala
    trait Exprs { this: Types =>
      type Expr[A] = c.Expr[A]

      object Expr extends ExprModule {
        // ...
      }
    }
    ```
 
!!! example "Scala 3-only codebase"

    ```scala
    abstract class DefinitionsPlatform(using val quotes: scala.quoted.Quotes)
        extends Definitions
        with TypesPlatform
        with ExprsPlatform
    ```
    
    ```scala
    trait TypesPlatform { this: DefinitionsPlatform =>
      type Type[A] = scala.quoted.Type[A]

      object Type extends TypeModule {
        // ...
      }
    }
    ```
    
    ```scala
    trait Exprs { this: Types =>
      type Expr[A] = scala.quoted.Expr[A]

      object Expr extends ExprModule {
        // ...
      }
    }
    ```

This simplified code shows the general idea behind Chimney's multiplatform architecture. For every feature we needed
in macros, we had to create an interface and then 2 platform-specific implementations. The biggest challenge was getting
abstractions right enough, but that was done iteratively. However, those are implementation details that shouldn't
matter to the user.

After defining the macro's logic with platform-independent code the only platform-specific code that has to be written
for each platform separately is plugging in the platform-specific implementations and the entrypoint to the macro:

  - Scala 2 has a concept of `c.prefix` storing expression with the content of whatever was before the dot
    (e.g. for `object.macroMethod` it would be an expression with `object`)
  - Scala 3 requires passing each expression explicitly, with no exceptions (so we would often pass `'{ this }` )

These adjustments are required to extract `source: From`, possibly `failFast: Boolean` and `RuntimeDataStore`, to be
able to pass them into macro.
