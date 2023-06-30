package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait SealedHierarchiesPlatform extends SealedHierarchies { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}
  import Type.platformSpecific.fromUntyped

  protected object SealedHierarchy extends SealedHierarchyModule {

    def isSealed[A](A: Type[A]): Boolean = {
      val sym = A.tpe.typeSymbol
      sym.isClass && sym.asClass.isSealed
    }

    type Subtype
    def parse[A: Type]: Option[Enum[A]] = if (isSealed(Type[A])) {
      val elements = extractSubclasses(Type[A].tpe.typeSymbol.asType).distinct
        .map { (subtype: TypeSymbol) =>
          subtypeName(subtype) -> subtypeTypeOf[A](subtype)
        }
        .filter { case (_, subtypeType) =>
          // with GADT we can have subtypes that shouldn't appear in pattern matching
          subtypeType <:< Type[A]
        }
        .map { case (subtypeName, subtypeType) =>
          implicit val Subtype: Type[Subtype] = subtypeType
          Existential[Enum.Element[A, *], Subtype](Enum.Element(name = subtypeName, upcast = _.upcastExpr[A]))
        }
      Some(Enum(elements))
    } else None

    private def extractSubclasses(t: TypeSymbol): List[TypeSymbol] =
      if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toList.map(_.asType).flatMap(extractSubclasses)
      else List(t)

    private def subtypeName(subtype: TypeSymbol): String = subtype.name.toString

    private def subtypeTypeOf[A: Type](subtype: TypeSymbol): Type[Subtype] = {
      val sEta = subtype.toType.etaExpand
      fromUntyped(
        sEta.finalResultType
          .substituteTypes(sEta.baseType(Type[A].tpe.typeSymbol).typeArgs.map(_.typeSymbol), Type[A].tpe.typeArgs)
      )
    }
  }
}
