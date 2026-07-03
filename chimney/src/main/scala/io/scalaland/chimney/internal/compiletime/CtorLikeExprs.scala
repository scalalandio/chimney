package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.partial

/** Maps Hearth `StandardMacroExtension` smart constructors (`hearth.std.CtorLikeOf`) onto
  * [[io.scalaland.chimney.partial.Result]]:
  *
  * {{{
  * EitherStringOrValue            : Left(string)   -> partial.Result.fromErrorString(string)
  * EitherIterableStringOrValue    : Left(strings)  -> partial.Result.fromErrorStrings(head, tail*)
  * EitherThrowableOrValue         : Left(throwable)-> partial.Result.fromErrorThrowable(throwable)
  * EitherIterableThrowableOrValue : Left(errors)   -> partial.Result.fromErrors(errors.map(Error.fromThrowable))
  * }}}
  *
  * The error is path-less at the construction site, like an `integrations.PartiallyBuildIterable` implicit's error
  * channel - the surrounding derivation prepends the usual paths.
  *
  * The `CtorLikeOf` shape is inspected once at PARSE time (never inside splice-invoked lambdas) and the returned
  * function only builds exprs.
  */
private[compiletime] trait CtorLikeExprs {
  this: ChimneyDefinitions & hearth.MacroCommons & hearth.std.StdExtensions =>

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

  // Left(empty-iterable) means failure even if the (malformed) provider passed no messages - fail explicitly.
  private val smartCtorNoErrorMessage = "Smart constructor failed without providing an error message"

  @scala.annotation.nowarn("msg=is never used")
  private def eitherStringToPartialResultExpr[A: Type](either: Expr[Either[String, A]]): Expr[partial.Result[A]] = {
    implicit val EitherStringA: Type[Either[String, A]] = Type.of[Either[String, A]]
    implicit val PartialResultA: Type[partial.Result[A]] = ChimneyType.PartialResult[A]
    Expr.quote {
      partial.Result.fromEitherString[A](Expr.splice(either))
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  private def eitherStringsToPartialResultExpr[A: Type](
      either: Expr[Either[Iterable[String], A]]
  ): Expr[partial.Result[A]] = {
    implicit val EitherStringsA: Type[Either[Iterable[String], A]] = Type.of[Either[Iterable[String], A]]
    implicit val PartialResultA: Type[partial.Result[A]] = ChimneyType.PartialResult[A]
    val noErrorMessage = Expr(smartCtorNoErrorMessage)
    Expr.quote {
      Expr
        .splice(either)
        .fold(
          errors =>
            if (errors.isEmpty) partial.Result.fromErrorString[A](Expr.splice(noErrorMessage))
            else partial.Result.fromErrorStrings[A](errors.head, errors.tail.toSeq*),
          value => partial.Result.fromValue[A](value)
        )
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  private def eitherThrowableToPartialResultExpr[A: Type](
      either: Expr[Either[Throwable, A]]
  ): Expr[partial.Result[A]] = {
    implicit val EitherThrowableA: Type[Either[Throwable, A]] = Type.of[Either[Throwable, A]]
    implicit val PartialResultA: Type[partial.Result[A]] = ChimneyType.PartialResult[A]
    Expr.quote {
      Expr
        .splice(either)
        .fold(
          throwable => partial.Result.fromErrorThrowable[A](throwable),
          value => partial.Result.fromValue[A](value)
        )
    }
  }

  @scala.annotation.nowarn("msg=is never used")
  private def eitherThrowablesToPartialResultExpr[A: Type](
      either: Expr[Either[Iterable[Throwable], A]]
  ): Expr[partial.Result[A]] = {
    implicit val EitherThrowablesA: Type[Either[Iterable[Throwable], A]] = Type.of[Either[Iterable[Throwable], A]]
    implicit val PartialResultA: Type[partial.Result[A]] = ChimneyType.PartialResult[A]
    val noErrorMessage = Expr(smartCtorNoErrorMessage)
    Expr.quote {
      Expr
        .splice(either)
        .fold(
          throwables =>
            if (throwables.isEmpty) partial.Result.fromErrorString[A](Expr.splice(noErrorMessage))
            else
              partial.Result.fromErrors[A](
                partial.Error.fromThrowable(throwables.head),
                throwables.tail.map(partial.Error.fromThrowable).toSeq*
              ),
          value => partial.Result.fromValue[A](value)
        )
    }
  }
}
