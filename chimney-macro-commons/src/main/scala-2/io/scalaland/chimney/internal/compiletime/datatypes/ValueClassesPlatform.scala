package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.collection.compat.*

trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Expr as _, Transformer as _, Type as _, *}
  import Type.platformSpecific.{fromUntyped, returnTypeOf}

  protected object ValueClassType extends ValueClassTypeModule {

    type Inner

    def parse[A: Type]: Option[Existential[ValueClass[A, *]]] = if (Type[A].isAnyVal && !Type[A].isPrimitive) {
      val A = Type[A].tpe

      val getter: Symbol = A.decls.to(List).find(m => m.isMethod && m.asMethod.isGetter).getOrElse {
        assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 parameter")
      }

      val primaryConstructor: Symbol = A.decls
        .to(List)
        .find(m => m.isPublic && m.isConstructor && m.asMethod.paramLists.flatten.size == 1)
        .getOrElse {
          assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 public constructor")
        }
      val argument = primaryConstructor.asMethod.paramLists.flatten.headOption.getOrElse {
        assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have public constructor with 1 argument")
      }

      implicit val Inner: Type[Inner] = fromUntyped[Inner](returnTypeOf(A, getter))
      assert(
        argument.typeSignature =:= Inner.tpe,
        s"AnyVal ${Type.prettyPrint[A]} only parameter's type was expected to be the same as only constructor argument's type"
      )

      val termName = getter.asMethod.name.toTermName

      Some(
        Existential(
          ValueClass[A, Inner](
            fieldName = getter.name.toString, // TODO: use utility from Products
            unwrap = (expr: Expr[A]) =>
              if (getter.asMethod.paramLists.isEmpty) c.Expr[Inner](q"$expr.$termName")
              else c.Expr[Inner](q"$expr.$termName()"),
            wrap = (expr: Expr[Inner]) => c.Expr[A](q"new $A($expr)")
          )
        )
      )
    } else None
  }
}
