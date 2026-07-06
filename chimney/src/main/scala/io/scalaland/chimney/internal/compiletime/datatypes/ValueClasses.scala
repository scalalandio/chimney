package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions
import io.scalaland.chimney.partial

/** Thin view over Hearth's `IsValueType` (built-in providers: `AnyVal`, Scala 3 opaque types, Java boxed primitives;
  * plus ServiceLoader-registered `StandardMacroExtension`s like chimney-protobufs' `Timestamp` provider), extended with
  * Chimney's own STRUCTURAL matching (any single-public-field class whose unambiguous unary public constructor matches
  * a same-named public getter - the `nonAnyValWrappers` feature, which no Hearth provider covers).
  *
  * Notes:
  *   - `IsValueType` is consulted FIRST (a provider match reflects the type author's/integration's intent and wins over
  *     structural accident); the Method-based parse remains as the structural fallback,
  *   - only `CtorLikeOf.PlainValue` wraps become a total [[WrapperClass]]; smart-constructor (validated) value types
  *     parse as [[PartialWrapperClass]] instead and surface only in PartialTransformer derivation (see
  *     [[io.scalaland.chimney.internal.compiletime.CtorLikeExprs]] for the `CtorLike` error -> `partial.Result`
  *     mapping),
  *   - `fieldName` (Path bookkeeping + error messages only) is taken from the wrap-method's parameter name when the
  *     provider supplies a `Method`, otherwise it defaults to `"value"`,
  *   - `AnyVal`s and the 8 Java boxed primitives surface through the UNGATED [[ValueClassType]]; boxed primitives are
  *     additionally pinned by the rules to their exact `boxed <-> primitive` pairs (see [[isJavaBoxedPrimitive]] - a
  *     DELIBERATE Chimney semantics decision, NOT a Hearth workaround: boxed types are nullable and eager structural
  *     unwrapping would preempt the null-safe Option rules),
  *   - other provider matches (opaque types, extensions) skip the `nonAnyValWrappers` flag (see
  *     [[WrapperClass.fromStdExtension]]): an opaque type is the Scala 3 value-type idiom and a registered provider is
  *     an explicit opt-in by the integration's author - the same trust level as an `integrations.TotallyBuildIterable`
  *     implicit; the flag keeps gating only the STRUCTURAL matching of arbitrary single-field classes.
  * The provider path calls `ensureStandardExtensionsLoaded()` (idempotent; the Gateways already load at entry), so the
  * datatypes layer stays safe even if consulted from a path that skipped a Gateway.
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
    *
    * @param fromStdExtension
    *   `true` when this wrapper was provided by a Hearth `IsValueType` extension provider (rather than matched
    *   STRUCTURALLY by the Method-based parse). The rules use it to skip the `nonAnyValWrappers` flag for
    *   extension-provided types: registering an `IsValueType` is an explicit opt-in by the integration's author
    *   (exactly like an `integrations.TotallyBuildIterable` implicit, which needs no flag either), while the flag
    *   guards the STRUCTURAL matching of arbitrary single-field classes (which could be surprising).
    */
  final protected case class WrapperClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      wrap: Expr[Inner] => Expr[Outer],
      fromStdExtension: Boolean = false
  )

  /** Let us unwrap and wrap value in `AnyVal` value class */
  final protected case class ValueClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      wrap: Expr[Inner] => Expr[Outer]
  )

  /** Like [[WrapperClass]] but for SMART-CONSTRUCTOR (validated) value types registered through Hearth `IsValueType`
    * extensions: unwrapping is total, wrapping can fail and therefore yields a `partial.Result` - only
    * PartialTransformer derivation can construct such types (the rules fail Total derivation with the usual meaningful
    * error).
    */
  final protected case class PartialWrapperClass[Outer, Inner](
      fieldName: String,
      unwrap: Expr[Outer] => Expr[Inner],
      partialWrap: Expr[Inner] => Expr[partial.Result[Outer]]
  )

  // Hoisted to the (unshadowed) trait level - Scala 2 cross-quotes expansions must not run inside scopes that shadow
  // the bare names the generated code references.
  private lazy val wrapperAnyValType: Type[AnyVal] = Type.of[AnyVal]
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
      hearthSupport[A].orElse(methodBasedParse[A])
    }
    def unapply[A](tpe: Type[A]): Option[Existential[WrapperClass[A, *]]] = parse(using tpe)

    /** Hearth `IsValueType` providers (built-ins AND `StandardMacroExtension`s) - see the trait's ScalaDoc. */
    private def hearthSupport[A: Type]: Option[Existential[WrapperClass[A, *]]] = {
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
                  wrap = plainValue.ctor.asInstanceOf[Expr[Inner] => Expr[A]],
                  // AnyVal/boxed matches flow through the ungated ValueClassType, so the flag-skipping marker is
                  // only meaningful (and only set) for opaque types and extension-provided value types.
                  fromStdExtension = !(Type[A] <:< wrapperAnyValType || isJavaBoxedPrimitive[A])
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
        getter <- (Type[A].unsortedMethods: List[Method])
          .collectFirst { // order-independent: name-matched unwrap getter
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
                .invoke(unambiguousConstructor)(None, Map(argumentName -> expr.as_??))
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
      // Java boxed primitives surface through the UNGATED ValueClassType: `java.lang.Integer` is
      // the Java spelling of an AnyVal, its Inner (`Int`) genuinely IS <: AnyVal, and the chimney-java-collections
      // implicits it replaces required no flag either. The actual unwrap/wrap exprs come from Hearth's built-in
      // `IsValueTypeProviderForJava*` through WrapperClassType.parse's provider fallback (the Method-based parse
      // rejects boxed types: ambiguous constructors and/or no matching getter names).
      if ((Type[A] <:< AnyValType) || isJavaBoxedPrimitive[A])
        WrapperClassType.parse[A].map {
          // The cast is a deliberate lie: the Inner type of an AnyVal does not have to be <: AnyVal (e.g. a String
          // field) - the rules rely on this loose upper bound.
          _.asInstanceOf[Existential.UpperBounded[AnyVal, WrapperClass[A, *]]].mapK[ValueClass[A, *]] { _ =>
            { case WrapperClass(fieldName, unwrap, wrap, _) =>
              ValueClass(fieldName, unwrap, wrap)
            }
          }
        }
      else None
    }
    def unapply[A](tpe: Type[A]): Option[Existential.UpperBounded[AnyVal, ValueClass[A, *]]] = parse(using tpe)
  }

  /** True for the 8 `java.lang` boxed primitives. Exposed for the rules: boxed primitives ARE [[ValueClassType]]s, but
    * UNLIKE real AnyVal wrappers they are NULLABLE, so the ValueClassToType/TypeToValueClass rules must only use them
    * for their EXACT primitive counterpart (`Integer <-> Int` etc.). Eager structural unwrapping for arbitrary targets
    * would preempt the null-SAFE ToOption/OptionToOption rules (e.g. `(null: Integer) -> Option[String]` must keep
    * producing `None`, not throw an NPE from `.intValue()`).
    */
  protected def isJavaBoxedPrimitive[A: Type]: Boolean =
    javaBoxedPrimitiveTypes.exists(boxed => Type[A] =:= boxed.Underlying)

  /** Smart-constructor value types: consulted by the rules AFTER [[ValueClassType]]/[[WrapperClassType]] both rejected
    * a type. Matches when a Hearth `IsValueType` provider exists whose `wrap` is one of the four smart-constructor
    * `CtorLikeOf` shapes - the CtorLike error channel maps onto `partial.Result` (see
    * [[io.scalaland.chimney.internal.compiletime.CtorLikeExprs]]), so such types can only be CONSTRUCTED by
    * PartialTransformers (unwrapping them as a source works in both).
    */
  protected object PartialWrapperClassType {

    private type Cached[A] = Option[Existential[PartialWrapperClass[A, *]]]
    private val partialWrapperClassCache = new TypeCache[Cached]
    def parse[A: Type]: Option[Existential[PartialWrapperClass[A, *]]] = partialWrapperClassCache(Type[A]) {
      // Total wrapping wins - a type that parses as a (provider-provided PlainValue or Method-based) WrapperClass
      // must keep its total expansion; smart-constructor support only ADDS types nothing else could handle.
      if (WrapperClassType.parse[A].isDefined) None
      else {
        ensureStandardExtensionsLoaded()
        IsValueType.unapply(Type[A]).flatMap { isValueType =>
          import isValueType.{Underlying as Inner, value as isValueTypeOf}
          val wrap = isValueTypeOf.wrap
          // The CtorLikeOf shape is inspected once, at parse level (returns None for PlainValue - those either
          // already matched WrapperClassType above or were rejected there for other reasons).
          ctorLikeToPartialResultExpr[Inner, A](wrap).map { partialWrap =>
            val fieldName = wrap.method
              .flatMap(method => method.parameters.flatten.headOption.map(_._1))
              .getOrElse("value")
            Existential[PartialWrapperClass[A, *], Inner](
              PartialWrapperClass[A, Inner](
                fieldName = fieldName,
                unwrap = isValueTypeOf.unwrap,
                partialWrap = partialWrap
              )
            )
          }
        }
      }
    }
    def unapply[A](tpe: Type[A]): Option[Existential[PartialWrapperClass[A, *]]] = parse(using tpe)
  }
}
