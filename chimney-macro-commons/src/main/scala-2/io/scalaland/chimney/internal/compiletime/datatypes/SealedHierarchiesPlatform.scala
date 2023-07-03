package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

trait SealedHierarchiesPlatform extends SealedHierarchies { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}
  import Type.platformSpecific.fromUntyped

  protected object SealedHierarchy extends SealedHierarchyModule {

    def isSealed[A](A: Type[A]): Boolean = {
      val sym = A.tpe.typeSymbol
      sym.isClass && sym.asClass.isSealed
    }

    type Subtype
    def parse[A: Type]: Option[Enum[A]] = if (isSealed(Type[A])) {
      // calling .distinct here as `knownDirectSubclasses` returns duplicates for multiply-inherited types
      val elements = extractSubclasses(Type[A].tpe.typeSymbol.asType).distinct
        .map { (subtype: TypeSymbol) =>
          val subtypeA = subtypeTypeOf[A](subtype)
          subtypeA.mapK[Enum.Element[A, *]] { implicit Subtype: Type[subtypeA.Underlying] => _ =>
            Enum.Element(name = subtypeName(subtype), upcast = _.upcastExpr[A])
          }
        }
        // with GADT we can have subtypes that shouldn't appear in pattern matching
        .filter(_.Underlying <:< Type[A])
      Some(Enum(elements))
    } else None

    private def extractSubclasses(t: TypeSymbol): List[TypeSymbol] =
      if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toList.map(_.asType).flatMap(extractSubclasses)
      else List(t)

    private def subtypeName(subtype: TypeSymbol): String = subtype.name.toString

    private def subtypeTypeOf[A: Type](subtype: TypeSymbol): ExistentialType.UpperBounded[A] = {
      subtype.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

      val sEta = subtype.toType.etaExpand
      fromUntyped[A](
        sEta.finalResultType.substituteTypes(
          sEta.baseType(Type[A].tpe.typeSymbol).typeArgs.map(_.typeSymbol),
          Type[A].tpe.typeArgs
        )
      ).asExistentialUpperBounded[A]
    }
  }
}
