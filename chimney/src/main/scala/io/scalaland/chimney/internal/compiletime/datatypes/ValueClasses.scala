package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions

/** Hearth-based port of the pre-Hearth `io.scalaland.chimney.internal.compiletime.datatypes.ValueClasses`.
  *
  * DESIGN CHOICE: implemented as a custom parse on Hearth's `Method` API (unambiguous unary public constructor +
  * same-named public getter) instead of Hearth's `IsValueType`:
  *   - Hearth's built-in providers cover only `AnyVal` (and Java boxed primitives) - macro-commons' `WrapperClassType`
  *     matches ANY single-public-field class with a matching constructor, so a custom provider would have been needed
  *     anyway,
  *   - `IsValueType` does not expose `fieldName` (used by the rules for `Path` bookkeeping),
  *   - it requires standard extensions to be loaded, which the plain-value datatypes layer should not depend on.
  *
  * EXTENSION FALLBACK (Phase 5 prereq): when the Method-based parse rejects a type, [[WrapperClassType.parse]]
  * additionally consults Hearth's `IsValueType` providers - this is how ServiceLoader-registered
  * `StandardMacroExtension`s (e.g. a future chimney-protobufs `Timestamp` <-> `Instant` provider) plug into the
  * value-class rules. Guards keeping CURRENT behavior byte-identical when no third-party extension is on the classpath
  * (i.e. Hearth 0.4.0 BUILT-IN `IsValueType` providers must never introduce a match the old engine did not have):
  *   - `A <:< AnyVal` is never consulted: `IsValueTypeProviderForAnyVal` (the only built-in that can match Scala
  *     classes) disagrees with the Method-based parse on edge cases (it uses `primaryConstructor` where we require an
  *     UNambiguous constructor, and it doesn't require the getter NAME to match the argument), so AnyVal handling stays
  *     100% with the Method-based parse (which already accepts all proper AnyVal wrappers),
  *   - the 8 `java.lang` boxed primitives are never consulted: `IsValueTypeProviderForJava*` would otherwise make e.g.
  *     `java.lang.Integer` a wrapper class (today it is not; chimney-java-collections provides Transformer implicits
  *     for boxed conversions and those must keep winning through rule #1),
  *   - Hearth exposes no provider provenance on a successful match (only skip-reasons carry provider names), so
  *     "extension-registered only" is enforced by the two type-level filters above, which exclude everything the Hearth
  *     0.4.0 built-in provider list can match. TODO(hearth-extensions): re-audit this list on every Hearth version bump
  *     (e.g. newer Hearth adds an opaque-type provider that these filters would NOT exclude),
  *   - only `CtorLikeOf.PlainValue` wraps are accepted - smart-constructor (validated) value types cannot be expressed
  *     as a total `WrapperClass`; TODO(hearth-extensions): support them in the partial rules via `CtorLikes`,
  *   - `fieldName` (Path bookkeeping + error messages only) is taken from the wrap-method's parameter name when the
  *     provider supplies a `Method`, otherwise it defaults to `"value"`,
  *   - note that the rules gate `WrapperClassType` matches behind the `nonAnyValWrappers` flag - extension-provided
  *     value types currently require `.enableNonAnyValWrappers` like any other non-AnyVal wrapper.
  *     TODO(hearth-extensions): Phase 5 should decide whether extension-registered types skip the flag.
  * The fallback calls `ensureStandardExtensionsLoaded()` (idempotent; the Gateways already load at entry), so the
  * datatypes layer stays safe even if consulted from a path that skipped a Gateway.
  *
  * Semantic judgment call: macro-commons' platforms disagreed on how the getter was found (Scala 3: first public
  * declaration; Scala 2: first public val accessor) and then required its name to match the constructor argument. Here
  * the getter is looked up directly by the constructor argument's name (public + nullary), which matches the documented
  * contract ("expose a getter of the same name and type as constructor's argument") and both platforms' behavior for
  * actual wrapper classes.
  */
private[compiletime] trait ValueClasses {
  this: ChimneyDefinitions & hearth.MacroCommons & hearth.std.StdExtensions =>

  /** Let us unwrap and wrap value in any class that wraps a single value (not only `AnyVal`s)
    *
    * For a class to be considered wrapper it has to:
    *   - have a public unary constructor
    *   - expose a getter of the same name and type as constructor's argument
    *
    * Basically, it is a value class without the need to extends AnyVal. This is useful since sometimes we have a type
    * which is basically a wrapper but not an `AnyVal` and we would like to unwrap it and attempt to derive code as if
    * it was `AnyVal`. Since it is very contextual, we need to have a separate utility for that.
    */
  final protected case class WrapperClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      wrap: Expr[Inner] => Expr[Outer]
  )

  /** Let us unwrap and wrap value in `AnyVal` value class */
  final protected case class ValueClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      wrap: Expr[Inner] => Expr[Outer]
  )

  // Hoisted to the (unshadowed) trait level like all other cross-quotes expansions - see the ScalaStdCompat GOTCHA.
  private lazy val wrapperAnyValType: Type[AnyVal] = Type.of[AnyVal]
  private lazy val wrapperBottomType: Type[Null] = Type.of[Null]
  private lazy val javaBoxedPrimitiveTypes: List[??] = List(
    Type.of[java.lang.Boolean].as_??,
    Type.of[java.lang.Byte].as_??,
    Type.of[java.lang.Character].as_??,
    Type.of[java.lang.Short].as_??,
    Type.of[java.lang.Integer].as_??,
    Type.of[java.lang.Long].as_??,
    Type.of[java.lang.Float].as_??,
    Type.of[java.lang.Double].as_??
  )

  protected object WrapperClassType {

    private type Cached[A] = Option[Existential[WrapperClass[A, *]]]
    private val wrapperClassCache = new TypeCache[Cached]
    def parse[A: Type]: Option[Existential[WrapperClass[A, *]]] = wrapperClassCache(Type[A]) {
      methodBasedParse[A].orElse(hearthProviderSupport[A])
    }
    def unapply[A](tpe: Type[A]): Option[Existential[WrapperClass[A, *]]] = parse(using tpe)

    /** Fallback consulting Hearth `IsValueType` providers registered by `StandardMacroExtension`s - see the trait's
      * ScalaDoc for the full list of guards and their rationale.
      */
    private def hearthProviderSupport[A: Type]: Option[Existential[WrapperClass[A, *]]] =
      // HEARTH GOTCHA (report upstream): bottom types conform to everything (`Null <:< java.lang.Integer` etc.), so
      // `<:<`-matching built-in providers match `Null`/`Nothing` and may CRASH eagerly while building their exprs.
      // Never consult providers for bottom types.
      if (Type[A] <:< wrapperBottomType) None
      else if (Type[A] <:< wrapperAnyValType) None // AnyVal stays with the Method-based parse (built-in excluded)
      else if (javaBoxedPrimitiveTypes.exists(boxed => Type[A] =:= boxed.Underlying)) None // built-ins excluded
      else {
        ensureStandardExtensionsLoaded()
        IsValueType.unapply(Type[A]).flatMap { isValueType =>
          import isValueType.{Underlying as Inner, value as isValueTypeOf}
          isValueTypeOf.wrap match {
            case plainValue: CtorLikeOf.PlainValue[?, ?] =>
              val fieldName = plainValue.method
                .flatMap(method => method.parameters.flatten.headOption.map(_._1))
                .getOrElse("value")
              Some(
                Existential[WrapperClass[A, *], Inner](
                  WrapperClass[A, Inner](
                    fieldName = fieldName,
                    unwrap = isValueTypeOf.unwrap,
                    wrap = plainValue.ctor.asInstanceOf[Expr[Inner] => Expr[A]]
                  )
                )
              )
            // Smart-constructor (validated) value types cannot become a total WrapperClass - see the trait's ScalaDoc.
            case _ => None
          }
        }
      }

    private def methodBasedParse[A: Type]: Option[Existential[WrapperClass[A, *]]] =
      for {
        unambiguousConstructor <- ProductType.unambiguousConstructorOf[A]
        argument <- unambiguousConstructor.totalParameters.flatten match {
          case argument :: Nil => Some(argument)
          case _               => None
        }
        (argumentName, argumentParam) = argument
        getter <- (Type[A].methods: List[Method]).collectFirst {
          case oi: Method.OnInstance
              if oi.name.trim == argumentName && oi.isNullary && oi.isAvailable(Everywhere) &&
                !oi.expectations.exists {
                  case MethodExpectation.NeedsTypes(_) => true
                  case _                               => false
                } =>
            (oi: Method)
        }
        if !Type.isPrimitive[A]
      } yield {
        val argumentType: ?? = argumentParam.tpe
        val inner: ?? = getter.knownReturning.getOrElse {
          // $COVERAGE-OFF$should never happen unless we messed up
          assertionFailed(s"Expected known return type of ${Type.prettyPrint[A]}'s getter $argumentName")
          // $COVERAGE-ON$
        }
        import inner.Underlying as Inner
        import argumentType.Underlying as Argument
        assert(
          Argument <:< Inner,
          s"Wrapper/AnyVal ${Type.prettyPrint[A]} only property's type (${Type
              .prettyPrint[Argument]}) was expected to be the same as only constructor argument's type (${Type.prettyPrint[Inner]})"
        )
        Existential[WrapperClass[A, *], Inner](
          WrapperClass[A, Inner](
            fieldName = argumentName,
            unwrap = (expr: Expr[A]) => ProductType.invokeNullaryInstanceMethod[A, Inner](getter)(expr),
            wrap = (expr: Expr[Inner]) =>
              ProductType
                .invokeMethodChain(unambiguousConstructor)(None, Map(argumentName -> expr.as_??))
                .fold(
                  error => assertionFailed(s"Failed to call constructor of ${Type.prettyPrint[A]}: $error"),
                  result => result.value.asInstanceOf[Expr[A]]
                )
          )
        )
      }
  }

  protected object ValueClassType {

    private lazy val AnyValType: Type[AnyVal] = Type.of[AnyVal]

    private type Cached[A] = Option[Existential.UpperBounded[AnyVal, ValueClass[A, *]]]
    private val valueClassCache = new TypeCache[Cached]
    def parse[A: Type]: Option[Existential.UpperBounded[AnyVal, ValueClass[A, *]]] = valueClassCache(Type[A]) {
      if (Type[A] <:< AnyValType)
        WrapperClassType.parse[A].map {
          // The cast is a macro-commons-inherited lie: the Inner type of an AnyVal does not have to be <: AnyVal
          // (e.g. a String field) - existing rules rely on this loose upper bound the same way macro-commons did.
          _.asInstanceOf[Existential.UpperBounded[AnyVal, WrapperClass[A, *]]].mapK[ValueClass[A, *]] { _ =>
            { case WrapperClass(fieldName, unwrap, wrap) =>
              ValueClass(fieldName, unwrap, wrap)
            }
          }
        }
      else None
    }
    def unapply[A](tpe: Type[A]): Option[Existential.UpperBounded[AnyVal, ValueClass[A, *]]] = parse(using tpe)
  }
}
