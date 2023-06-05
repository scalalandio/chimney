package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  protected object ValueClass extends ValueClassModule {

    def unapply[A](implicit A: Type[A]): Option[ValueClass[A]] = if (A.isAnyVal && !A.isPrimitive) {
      Some(
        new ValueClass[A] {
          private val getter: Symbol = A.decls.to(List).find(m => m.isMethod && m.asMethod.isGetter).getOrElse {
            assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 parameter")
          }

          private val primaryConstructor: Symbol = A.decls
            .to(List)
            .find(m => m.isPublic && m.isConstructor && m.asMethod.paramLists.flatten.size == 1)
            .getOrElse {
              assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have 1 public constructor")
            }
          private val argument = primaryConstructor.asMethod.paramLists.flatten.headOption.getOrElse {
            assertionFailed(s"AnyVal ${Type.prettyPrint[A]} expected to have public constructor with 1 argument")
          }

          val Inner: Type[Inner] = Type.platformSpecific.returnTypeOf(A, getter).asInstanceOf[Type[Inner]]
          assert(
            argument.typeSignature.asInstanceOf[Type[Inner]] =:= Inner,
            s"AnyVal ${Type.prettyPrint[A]} only parameter's type was expected to be the same as only constructor argument's type"
          )

          val fieldName: String = getter.name.toString
          private val termName = getter.asMethod.name.toTermName

          def unwrap(expr: Expr[A]): Expr[Inner] =
            if (getter.asMethod.paramLists.isEmpty) c.Expr[Inner](q"$expr.$termName")
            else c.Expr[Inner](q"$expr.$termName()")

          def wrap(expr: Expr[Inner]): Expr[A] = c.Expr[A](q"new $A($expr)")
        }
      )
    } else None
  }
}
