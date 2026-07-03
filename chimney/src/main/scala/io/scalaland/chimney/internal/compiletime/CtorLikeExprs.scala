package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.partial

/** Maps Hearth `StandardMacroExtension` SMART CONSTRUCTORS (`hearth.std.CtorLikeOf`) onto Chimney's
  * [[io.scalaland.chimney.partial.Result]] semantics (Phase 5 engine prerequisite).
  *
  * Hearth 0.4.0 providers describe how a value of a type is constructed with a `CtorLikeOf[Input, Output]`:
  *   - `PlainValue` - a TOTAL constructor; handled by the total paths (`TotallyBuildIterable`/`WrapperClassType`
  *     fallbacks) and deliberately returns `None` here,
  *   - `EitherStringOrValue`/`EitherIterableStringOrValue`/`EitherThrowableOrValue`/`EitherIterableThrowableOrValue` -
  *     VALIDATED ("smart") constructors, e.g. Kindlings cats-integration's `NonEmptyList` provider returns
  *     `Left("Cannot create NonEmptyList from empty collection")` for an empty builder. These cannot be total, so they
  *     surface in the PARTIAL paths ([[derivation.transformer.integrations.PartiallyBuildIterables]]'s extension
  *     fallback and [[datatypes.ValueClasses]]' `PartialWrapperClassType`), where the error channel maps onto
  *     `partial.Result` exactly like the existing `io.scalaland.chimney.integrations.PartiallyBuildIterable` implicits'
  *     error channel does (an error - path-less at the construction site - which the surrounding derivation then
  *     annotates with the usual paths):
  *     {{{
  *     EitherStringOrValue            : Left(string)   -> partial.Result.fromErrorString(string)
  *     EitherIterableStringOrValue    : Left(strings)  -> partial.Result.fromErrorStrings(head, tail*)
  *     EitherThrowableOrValue         : Left(throwable)-> partial.Result.fromErrorThrowable(throwable)
  *     EitherIterableThrowableOrValue : Left(errors)   -> partial.Result.Errors(errors.map(Error.fromThrowable))
  *     }}}
  *     The mapping itself happens at RUNTIME through [[io.scalaland.chimney.internal.runtime.SmartConstructorResults]]
  *     (generated code applies the provider's constructor expr and adapts its `Either`).
  *
  * The `CtorLikeOf` shape is inspected once at PARSE time (never inside splice-invoked lambdas - the cross-quotes
  * lesson from the java-collections work) and the returned function only builds exprs.
  */
private[compiletime] trait CtorLikeExprs {
  this: ChimneyDefinitions & hearth.MacroCommons & hearth.std.StdExtensions =>

  // Hoisted to the (unshadowed) trait level like all other cross-quotes expansions - see the ScalaStdCompat GOTCHA.
  private lazy val ctorLikeStringType: Type[String] = Type.of[String]
  private lazy val ctorLikeThrowableType: Type[Throwable] = Type.of[Throwable]
  private lazy val ctorLikeStringsType: Type[Iterable[String]] = Type.of[Iterable[String]]
  private lazy val ctorLikeThrowablesType: Type[Iterable[Throwable]] = Type.of[Iterable[Throwable]]

  /** Turns a Hearth smart constructor into an `Input => partial.Result[Output]` expr-level function.
    *
    * Returns `None` for `CtorLikeOf.PlainValue` (total constructors are the total paths' business) - so a `Some(...)`
    * result is also the "this provider is smart-constructor-shaped" test used by the partial fallbacks.
    */
  protected def ctorLikeToPartialResultExpr[Input, Output: Type](
      ctorLike: CtorLikeOf[Input, Output]
  ): Option[Expr[Input] => Expr[partial.Result[Output]]] = ctorLike match {
    case _: CtorLikeOf.PlainValue[?, ?]                                           => None
    case esv: CtorLikeOf.EitherStringOrValue[Input @unchecked, Output @unchecked] =>
      Some(input => eitherStringToPartialResultExpr[Output](esv.ctor(input)))
    case eisv: CtorLikeOf.EitherIterableStringOrValue[Input @unchecked, Output @unchecked] =>
      Some(input => eitherStringsToPartialResultExpr[Output](eisv.ctor(input)))
    case etv: CtorLikeOf.EitherThrowableOrValue[Input @unchecked, Output @unchecked] =>
      Some(input => eitherThrowableToPartialResultExpr[Output](etv.ctor(input)))
    case eitv: CtorLikeOf.EitherIterableThrowableOrValue[Input @unchecked, Output @unchecked] =>
      Some(input => eitherThrowablesToPartialResultExpr[Output](eitv.ctor(input)))
  }

  // Cross-quotes helper defs with regular type parameters (the documented helper-def pattern).

  @scala.annotation.nowarn("msg=is never used")
  private def eitherStringToPartialResultExpr[A: Type](either: Expr[Either[String, A]]): Expr[partial.Result[A]] = {
    implicit val EitherStringA: Type[Either[String, A]] = ScalaType.Either[String, A](using ctorLikeStringType, Type[A])
    implicit val PartialResultA: Type[partial.Result[A]] = ChimneyType.PartialResult[A]
    Expr.quote(
      io.scalaland.chimney.internal.runtime.SmartConstructorResults.fromEitherString(Expr.splice(either))
    )
  }

  @scala.annotation.nowarn("msg=is never used")
  private def eitherStringsToPartialResultExpr[A: Type](
      either: Expr[Either[Iterable[String], A]]
  ): Expr[partial.Result[A]] = {
    implicit val EitherStringsA: Type[Either[Iterable[String], A]] =
      ScalaType.Either[Iterable[String], A](using ctorLikeStringsType, Type[A])
    implicit val PartialResultA: Type[partial.Result[A]] = ChimneyType.PartialResult[A]
    Expr.quote(
      io.scalaland.chimney.internal.runtime.SmartConstructorResults.fromEitherStrings(Expr.splice(either))
    )
  }

  @scala.annotation.nowarn("msg=is never used")
  private def eitherThrowableToPartialResultExpr[A: Type](
      either: Expr[Either[Throwable, A]]
  ): Expr[partial.Result[A]] = {
    implicit val EitherThrowableA: Type[Either[Throwable, A]] =
      ScalaType.Either[Throwable, A](using ctorLikeThrowableType, Type[A])
    implicit val PartialResultA: Type[partial.Result[A]] = ChimneyType.PartialResult[A]
    Expr.quote(
      io.scalaland.chimney.internal.runtime.SmartConstructorResults.fromEitherThrowable(Expr.splice(either))
    )
  }

  @scala.annotation.nowarn("msg=is never used")
  private def eitherThrowablesToPartialResultExpr[A: Type](
      either: Expr[Either[Iterable[Throwable], A]]
  ): Expr[partial.Result[A]] = {
    implicit val EitherThrowablesA: Type[Either[Iterable[Throwable], A]] =
      ScalaType.Either[Iterable[Throwable], A](using ctorLikeThrowablesType, Type[A])
    implicit val PartialResultA: Type[partial.Result[A]] = ChimneyType.PartialResult[A]
    Expr.quote(
      io.scalaland.chimney.internal.runtime.SmartConstructorResults.fromEitherThrowables(Expr.splice(either))
    )
  }
}
