package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.collection.compat.*

private[compiletime] trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Expr as _, Transformer as _, Type as _, *}
  import Type.platformSpecific.returnTypeOf, Expr.platformSpecific.asExpr

  protected object ValueClassType extends ValueClassTypeModule {

    type Inner

    def parse[A: Type]: Option[Existential[ValueClass[A, *]]] = if (Type[A].isAnyVal && !Type[A].isPrimitive) {
      val getter: Symbol = Type[A].decls.to(List).find(m => m.isMethod && m.asMethod.isGetter).getOrElse {
        assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 parameter")
      }

      val primaryConstructor: Symbol = Type[A].decls
        .to(List)
        .find(m => m.isPublic && m.isConstructor && m.asMethod.paramLists.flatten.size == 1)
        .getOrElse {
          assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 public constructor")
        }
      val argument = primaryConstructor.asMethod.paramLists.flatten.headOption.getOrElse {
        assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have public constructor with 1 argument")
      }

      implicit val Inner: Type[Inner] = returnTypeOf(Type[A], getter).asInstanceOf[Type[Inner]]
      assert(
        argument.typeSignature.asInstanceOf[Type[Inner]] =:= Inner,
        s"AnyVal ${Type.prettyPrint[A]} only parameter's type was expected to be the same as only constructor argument's type"
      )

      val termName = getter.asMethod.name.toTermName

      Some(
        Existential(
          ValueClass[A, Inner](
            fieldName = getter.name.toString,
            unwrap = (expr: Expr[A]) =>
              if (getter.asMethod.paramLists.isEmpty) asExpr[Inner](q"$expr.$termName")
              else asExpr[Inner](q"$expr.$termName()"),
            wrap = (expr: Expr[Inner]) => asExpr[A](q"new ${Type[A]}($expr)")
          )
        )
      )
    } else None
  }
}
