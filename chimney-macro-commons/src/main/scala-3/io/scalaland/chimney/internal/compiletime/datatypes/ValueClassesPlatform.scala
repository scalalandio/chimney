package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*
  import Type.platformSpecific.*

  protected object WrapperClassType extends WrapperClassTypeModule {

    def parse[A: Type]: Option[Existential[WrapperClass[A, *]]] = {
      val A: TypeRepr = TypeRepr.of[A]
      val sym: Symbol = A.typeSymbol

      val getterOpt: Option[Symbol] = sym.declarations.filter(isPublic).headOption
      val primaryConstructorOpt: Option[Symbol] = Option(sym.primaryConstructor).filter(_.isClassConstructor)
      val argumentOpt: Option[Symbol] = primaryConstructorOpt.flatMap { primaryConstructor =>
        paramListsOf(primaryConstructor).flatten match {
          case argument :: Nil => Some(argument)
          case _               => None
        }
      }

      (getterOpt, primaryConstructorOpt, argumentOpt) match {
        case (Some(getter), Some(primaryConstructor), Some(argument))
            if !Type[A].isPrimitive && getter.name == argument.name =>
          val Argument =
            paramsWithTypes(A, primaryConstructor, isConstructor = true)(argument.name).asType.asInstanceOf[Type[Any]]
          val inner = returnTypeOf[Any](A.memberType(getter)).as_??
          import inner.Underlying as Inner
          assert(
            Argument =:= Inner,
            s"Wrapper/AnyVal ${Type.prettyPrint[A]} only property's type (${Type
                .prettyPrint(Argument)}) was expected to be the same as only constructor argument's type (${Type
                .prettyPrint(Inner)})"
          )
          Some(
            Existential[WrapperClass[A, *], Inner](
              WrapperClass[A, Inner](
                fieldName = getter.name,
                unwrap = (expr: Expr[A]) => expr.asTerm.select(getter).appliedToArgss(Nil).asExprOf[Inner],
                wrap = (expr: Expr[Inner]) => {
                  val select = New(TypeTree.of[A]).select(primaryConstructor)
                  val tree = if A.typeArgs.nonEmpty then select.appliedToTypes(A.typeArgs) else select
                  tree.appliedToArgss(List(List(expr.asTerm))).asExprOf[A]
                }
              )
            )
          )
        case _ => None
      }
    }

    private def isPublic(sym: Symbol): Boolean =
      !(sym.flags.is(Flags.Private) || sym.flags.is(Flags.PrivateLocal) || sym.flags.is(Flags.Protected))
  }
}
