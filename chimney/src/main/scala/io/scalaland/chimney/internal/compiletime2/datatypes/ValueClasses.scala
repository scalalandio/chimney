package io.scalaland.chimney.internal.compiletime2.datatypes

import io.scalaland.chimney.internal.compiletime2.ChimneyDefinitions

/** Hearth-based port of `io.scalaland.chimney.internal.compiletime.datatypes.ValueClasses`.
  *
  * DESIGN CHOICE: implemented as a custom parse on Hearth's `Method` API (unambiguous unary public constructor +
  * same-named public getter) instead of Hearth's `IsValueType`:
  *   - Hearth's built-in providers cover only `AnyVal` (and Scala 3 opaque types) - macro-commons' `WrapperClassType`
  *     matches ANY single-public-field class with a matching constructor, so a custom provider would have been needed
  *     anyway,
  *   - `IsValueType` does not expose `fieldName` (used by the rules for `Path` bookkeeping),
  *   - `IsValueType` would additionally match Scala 3 opaque types, which macro-commons did NOT treat as value classes -
  *     that would silently change rule behavior,
  *   - it requires standard extensions to be loaded, which the plain-value datatypes layer should not depend on.
  * TODO(hearth-migration): reconsider exposing Hearth's `IsValueType` (opaque types, smart constructors) as a new
  * feature after the flip.
  *
  * Semantic judgment call: macro-commons' platforms disagreed on how the getter was found (Scala 3: first public
  * declaration; Scala 2: first public val accessor) and then required its name to match the constructor argument. Here
  * the getter is looked up directly by the constructor argument's name (public + nullary), which matches the documented
  * contract ("expose a getter of the same name and type as constructor's argument") and both platforms' behavior for
  * actual wrapper classes.
  */
private[compiletime2] trait ValueClasses { this: ChimneyDefinitions & hearth.MacroCommons =>

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

  protected object WrapperClassType {

    private type Cached[A] = Option[Existential[WrapperClass[A, *]]]
    private val wrapperClassCache = new TypeCache[Cached]
    def parse[A: Type]: Option[Existential[WrapperClass[A, *]]] = wrapperClassCache(Type[A]) {
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
    def unapply[A](tpe: Type[A]): Option[Existential[WrapperClass[A, *]]] = parse(using tpe)
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
