package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

trait SealedHierarchiesPlatform extends SealedHierarchies { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}
  import Type.platformSpecific.*

  protected object SealedHierarchy extends SealedHierarchyModule {

    def isSealed[A](A: Type[A]): Boolean = {
      val sym = A.tpe.typeSymbol
      sym.isClass && sym.asClass.isSealed
    }

    def isJavaEnum[A](A: Type[A]): Boolean = A.tpe.typeSymbol.isJavaEnum

    def parse[A: Type]: Option[Enum[A]] =
      if (isSealed(Type[A])) Some(symbolsToEnum(extractSubclasses(Type[A].tpe.typeSymbol.asType)))
      else if (isJavaEnum(Type[A])) Some(symbolsToEnum(extractJavaEnumInstances(Type[A].tpe)))
      else None

    private def extractSubclasses(t: TypeSymbol): List[TypeSymbol] =
      if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toList.map(_.asType).flatMap(extractSubclasses)
      else List(t)

    private def extractJavaEnumInstances(t: c.Type): List[TypeSymbol] =
      t.companion.decls.filter(_.isJavaEnum).map(_.asType).toList

    private def symbolsToEnum[A: Type](subtypes: List[TypeSymbol]): Enum[A] =
      Enum(
        // calling .distinct here as `knownDirectSubclasses` returns duplicates for multiply-inherited types
        subtypes.distinct
          .map { (subtype: TypeSymbol) =>
            val subtypeA = subtypeTypeOf[A](subtype)
            subtypeA.mapK[Enum.Element[A, *]] { implicit Subtype: Type[subtypeA.Underlying] => _ =>
              Enum.Element(name = subtypeName(subtype), upcast = _.upcastExpr[A])
            }
          }
          // with GADT we can have subtypes that shouldn't appear in pattern matching
          .filter(_.Underlying <:< Type[A])
      )

    private def subtypeName(subtype: TypeSymbol): String = subtype.name.toString
  }
}
