package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

import scala.collection.compat.*

trait ValueClassesPlatform extends ValueClasses { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Expr as _, Transformer as _, Type as _, *}
  import Type.platformSpecific.*

  protected object WrapperClassType extends WrapperClassTypeModule {

    def parse[A: Type]: Option[Existential[WrapperClass[A, *]]] = {
      val A = Type[A].tpe

      val getterOpt: Option[Symbol] = A.decls.to(List).find(m => m.isPublic && m.isMethod && m.asMethod.isGetter)
      val primaryConstructorOpt: Option[Symbol] = Option(A.typeSymbol)
        .filter(_.isClass)
        .map(_.asClass.primaryConstructor)
        .find(m => m.isPublic && m.isConstructor && m.asMethod.paramLists.flatten.size == 1)
      val argumentOpt: Option[Symbol] = primaryConstructorOpt.flatMap(_.asMethod.paramLists.flatten.headOption)

      (getterOpt, primaryConstructorOpt, argumentOpt) match {
        case (Some(getter), Some(pCtor), Some(argument))
            if !Type[A].isPrimitive && getDecodedName(getter) == getDecodedName(argument) =>
          val PCtor = pCtor.typeSignatureIn(A).asInstanceOf[MethodType]
          val Argument = fromUntyped[Any](PCtor.params.head.typeSignatureIn(PCtor))
          val inner = fromUntyped(returnTypeOf(A, getter)).as_??
          import inner.Underlying as Inner
          assert(
            Argument <:< Inner,
            s"Wrapper/AnyVal ${Type.prettyPrint[A]} only property's type (${Type
                .prettyPrint(Argument)}) was expected to be the same as only constructor argument's type (${Type
                .prettyPrint(Inner)})"
          )

          val termName = getter.asMethod.name.toTermName

          Some(
            Existential[WrapperClass[A, *], Inner](
              WrapperClass[A, Inner](
                fieldName = getDecodedName(getter),
                unwrap = (expr: Expr[A]) =>
                  if (getter.asMethod.paramLists.isEmpty) c.Expr[Inner](q"$expr.$termName")
                  else c.Expr[Inner](q"$expr.$termName()"),
                wrap = (expr: Expr[Inner]) => c.Expr[A](q"new $A($expr)")
              )
            )
          )
        case _ => None
      }
    }

    private val getDecodedName = (s: Symbol) => s.name.decodedName.toString
  }
}
