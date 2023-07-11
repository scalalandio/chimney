package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.collection.compat.*

trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Expr as _, Transformer as _, Type as _, *}
  import Type.platformSpecific.{fromUntyped, returnTypeOf}

  protected object WrapperClassType extends WrapperClassTypeModule {

    def parse[A: Type]: Option[Existential[WrapperClass[A, *]]] = {
      val A = Type[A].tpe

      val getterOpt: Option[Symbol] = A.decls.to(List).find(m => m.isMethod && m.asMethod.isGetter)
      val primaryConstructorOpt: Option[Symbol] = A.decls
        .to(List)
        .find(m => m.isPublic && m.isConstructor && m.asMethod.paramLists.flatten.size == 1)
      val argumentOpt: Option[Symbol] = primaryConstructorOpt.flatMap(_.asMethod.paramLists.flatten.headOption)

      (getterOpt, primaryConstructorOpt, argumentOpt) match {
        case (Some(getter), Some(_), Some(argument)) if !Type[A].isPrimitive =>
          val inner = fromUntyped(returnTypeOf(A, getter)).asExistential
          import inner.Underlying as Inner
          assert(
            argument.typeSignature =:= inner.Underlying.tpe,
            s"Wrapper/AnyVal ${Type.prettyPrint[A]} only parameter's type was expected to be the same as only constructor argument's type"
          )

          val termName = getter.asMethod.name.toTermName

          Some(
            Existential[WrapperClass[A, *], inner.Underlying](
              WrapperClass[A, inner.Underlying](
                fieldName = getter.name.toString, // TODO: use utility from Products
                unwrap = (expr: Expr[A]) =>
                  if (getter.asMethod.paramLists.isEmpty) c.Expr[inner.Underlying](q"$expr.$termName")
                  else c.Expr[inner.Underlying](q"$expr.$termName()"),
                wrap = (expr: Expr[inner.Underlying]) => c.Expr[A](q"new $A($expr)")
              )
            )
          )
        case _ => None
      }
    }
  }
}
