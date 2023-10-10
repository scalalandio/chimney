# Under the Hood

If you think that what Chimney does is magic, it might help you if you learn how exactly expressions are generated.

This section aims to explain some mechanics of Chimney, at least those that might affect users and aren't just
implementation detail.

## Introduction

Chimney's behavior was designed with users' convenience in mind, starting with the best draft of an API and tests and
then figuring out a way to implement such behavior. Over the time, with feedback, we had a larger and larger list of
things that users considered to be intuitive (or at least safe, especially for corner cases), which we could use as
a guidance, iteratively improving the design.

Oftentimes, we received conflicting opinions about what is intuitive (or safe) and we had to decide ourselves how to
turn them into a deterministic specification. Which is why some of the decisions here will sound arbitrary. Yet they are
deterministic and tested with specs on every supported version of Scala on every platform.

In order to understand the mechanics of Chimney, we need to discuss which things are calculated within DSL, when calling
fluent API, and which are done within macros. Which things needs to be computed in the runtime and which can be computed
completely in the compile time. We will discuss each of these, recalling the reasons behind design decisions.

## DSL

How users call the API and what they expect to happen, enforces a lot of design decisions. Here, we'll analyze what
happens in DSL, when we're triggering automatic and semiautomatic derivation or customizing the transformer with flags
or overrides.

### How DSL summons `Transformer` instance

Users' expectations start at the attempt to transform a value without any customization.

!!! example

    ```scala
    import io.scalaland.chimney.dsl._
   
    val source: Source = ...
    source.transformInto[Target]
    ```
    
    The code above should:
    
      - use `Transformer[Source, Target]` if the user provided it
      - automatically provide a transformation if and only if user didn't provide it
      - generate a readable error message if it cannot be generated (_implicit not found_ doesn't make the cut)

It sounds like `transformInto` should be a macro, but someone might also want to use it in a similar manner to other
libraries with (automatic) derivation.

!!! example

    Passing `Transformer` with short lambda syntax

    ```scala
    val customTransformers: List[Transformer[Source, Target]] = ...
    val source: Source = ...
    list.map(source.transformInto(_))
    ```

    Passing `Transformer` manually

    ```scala
    val transformer: Transformer[Source, Target]]
    val source: Source
    source.pipe(_.transformInto(transformer)).pipe(println)
    ```

These aren't common cases, but for the debugging reasons sometimes you may want to a pass value explicitly. You wouldn't
do that with a macro which summons implicit internally. 

So, should the extension method look like this?

!!! example

    ```scala
    // extension method on [From](from: From)
    def transformInto[To](implicit transformer: Transformer[From, To]): To = transformer.transform(from)
    ```

Well, to make it work with automatic derivation, we would have to put `Transformer[From, To]` into implicit scope.
We'll get to details later, but for now: in macros we  **never** want to summon implicits that
**the user did not provide**, we **always** want to be able to summon implicit that **the user did provide**.
If users are supposed to provide their own instances with `implicit` `Transformer`s and automatic derivation would also
provide an `implicit` `Transformer`, how we would distinct user's instances from automatic ones? The solution is to make
them slightly different types:

!!! example

    ```scala
    // Transformer is a subtype of Autoderived (is more specific/constrained)
    trait Transformer[From, To] extends Transformer.Autoderived {
      def transform(src: From): To   
    }
    object Transformer {
      trait Autoderived[From, To] {
        def transform(src: From): To   
      }
      // Somewhere in the companion object we would define an implicit derivation of Autoderived,
      // making it a low-priority(!!!) implicit and a macro.
    }
    ```
    
    ```scala
    // extension method on [From](from: From)
    def transformInto[To](implicit transformer: Transformer.Autoderived[From, To]): To = transformer.transform(from)
    ``` 

With such setup:

  - the user's `Transformer` would always be attempted first
  - then summoning would fall back on `Transformer.Autoderived` if there was none (being low-priority) 
  - since the automatic derivation would be a macro, it could produce a nice compiler error instead of
    _implicit not found_ (in macros there are ways to fail compilation with an error)
  - when needed, we could rule out automatic derivation just by summoning `Transformer` instead of
    `Transformer.Autoderived` (which is done both in macros and in
    [`import io.scalaland.chimney.syntax._`](cookbook.md#automatic-semiautomatic-and-inlined-derivation))

### How DSL summons `PartialTransformer` instance

`PartialTransformer`s are virtually the same as `Transformer`s. The only difference is that you should always be able to
perform a partial transformation when there is `Transformer`, so the priority order of implicit summoning looks like
this:

  - the compiler attempts to find an implicit `PartialTransformer` provided by the user
  - then attempts to find an implicit `Transformer` provided by the user and lift it to a `PartialTransformer`
  - finally attempts automatic derivation with a macro

### How customization works

The API call to customize the derivation could like this:

!!! example

    ```scala
    val source: Source = ...
    source.into[Target].withFieldConst(_.a, value).enableMethodAccessors.transform
    ```

It would have to:

  - ignore and existing `Transformer[From, To]` if there is one (if you wanted existing one, you wouldn't customize it)
  - make sure that `value` would be computed exactly once (in API we are passing computed value to a method, if it
    suddenly becomes a AST tree which we would copy-pasting around it could change the behavior of a program)
  - make sure that value is used only for `a` field of `Target`
  - and that derivation would not forget that Method Accessors flag were turned on
  - somehow `source` would have to saved - we cannot lose it in between all these chained calls 

#### Carrying around the runtime configuration

How to achieve it with the summoning of an implicit? Not easily. This implicit summoning would have to somehow carry
all the necessary information to create a `Transformer` in the type and also carry around `source` and `value`.

It is much easier to just skip the part where you create a `Transformer`, instantiate it and pass into it all the
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
    
    Actually, when you call macros, the whole engine generates expression of type `To` (or `partial.Result[To]`).
    `source.into[Target].transform` inlines this generated expression directly, while automatic derivation,
    `Transformer.derive[From, To]` and `Transformer.define[From, To].buildTransformer` additionally:
    
    ```scala
    new Transformer[From, To] {
      def transform(src: From): To = 
        // use `src` as source expression for derivation (Expr[From])
        ${ derivedToExpression: Expr[To] } // paste the derived expression as Transformer's `transform` body
    }
    ```
    
    So only methods actually demanding a type class wrap the derived expression with it, internally almost all the time
    Chimney build just a `To` expression which attempt to compute the value in the leanest way possible.

So the next question is: how to pass to it all runtime values and configurations collected on the way from `.into` till
`.transform`?

Well. It appears that runtime values are the easiest to store in runtime. When we call `.into` we are creating a wrapper

!!! example

    ```scala
    class TransformerInto(...) {
      // ...
    }
    // extension method on [From](source: From)
    def into[To]: TransformerInto[From, To] = new TransformerInto[From, To](...)
    ```

    (`PartialTransformer` has its own `PartialTransformerInto`).

This is unavoidable, after all we have to define all these `.withField*`, `.withCoproduct*`, `.enable*`, `.disable*` and
`.transform` define on something. So this something happens to be a `TransformerInto` class - it exists at runtime, so
it is a great candidate to pass around `From` value for a future consumption. But `From` is a fixed type. What about
all these constants and functions that we also need to carry around? Well, inside `TransformerInto` there is also
another value, let's call it `RuntimeDataStore` which collects all these values as we chain methods. Upcasting them
to a common supertype: `Any`. So from the JVM perspective, after type erasure, what we have is similar to:

!!! example

    ```scala
    new TransformerInto(source)
      .methodAppendingValue(constant: Any)
      .methodAppendingValue(function: Any)
      ...
    ```

At the end of this chain is an object which the macro (from `.transform`) can access to extract from it `From` value,
and `RuntimeDataStore` value (currently `type RuntimeDataStore = Vector[Any]`). It means, that as long as macro would
know what is stored at each `RuntimeDataStore` index, and what is the runtime type of the value, it would have all the
information we provided it.

#### Carrying around the type-level configuration

The easiest way to store this kind of information was by introducing a phantom type (a type used only during
compilation) which represented configuration as a list of sort: you start with a type representing an empty config,
and then you prepend each config similar to how cons works in normal list. You end up with something like:

!!! example

    ```scala
    source.withFieldConst(_.a, ...).withFieldRenamed(_.b, _.c).transform
    // has a Cfg type like
    TransformerCfg.FieldRenamed[fieldBType, fieldCType, TransformerCfg.FieldConst[fieldAType, TransformerCfg.Empty]]
    // Let's not dive into how field names are represented. 
    ```

This type is build by whitebox macros (on Scala 2) or transparent inline def (Scala 3) - macros would read the tree of
the `_.fieldName`, extract `fieldName` out of it, turn it into a type and put inside the new config type. (These macros
would also append the constant/function to `RuntimeDataStore`).

The macro is able to parse this type, recovering the order in which types were applied, the name of each field, the
type of each enum's subtype, etc. From that it can calculate on which index each **override** is stored.

Not everything though has to be stored in runtime. If you take a look at the source code you'll notice that there is
another type, using similar tricks to `TransformerCfg`, called `TransformerFlags`. It is used to carry around settings
(in the form of **flags**), that has no relation to a particular type, but still drives the macro logic. Contrary to
`TransformerCfg` flags can also be shared: you can create an implicit `TransformerConfiguration` - this is the only
implicit always looked for by:

  - automatic derivation
  - semiautomatic derivation
  - inlined derivation (`into.transform`)

And since it is an implicit, it can be shared between several different macro expansions.

!!! example

    ```scala
    implicit val cfg = TransformerConfiguration.default.enableMacrosLogging
    
    "test".transformInto[Option[String]]
    "test".into[Option[String]].transform
    ```

`PartialTransformer`s are virtually identical to `Transformers` when it comes to this mechanics. They have a few more
configs, meaning that they need a separate builder, but they use the same `TransformerCfg` and `TransformerFlags`. 

### Semiautomatic derivation

TODO

## How macros generate output

TODO

### Setup

TODO config parsing

TODO expr calculation

TODO context - total vs partial

### Derivation rules

TODO

### Recursion

TODO

### Error handling and logging

TODO
