package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

trait SealedHierarchiesPlatform extends SealedHierarchies { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}
  import Type.platformSpecific.*

  protected object SealedHierarchy extends SealedHierarchyModule {

    def isJavaEnum[A: Type]: Boolean =
      Type[A].tpe.typeSymbol.isJavaEnum

    def isSealed[A: Type]: Boolean = {
      val sym = Type[A].tpe.typeSymbol
      sym.isClass && sym.asClass.isSealed
    }

    def parse[A: Type]: Option[Enum[A]] =
      if (isJavaEnum[A]) Some(symbolsToEnum(extractJavaEnumInstances[A]))
      else if (isSealed[A]) Some(symbolsToEnum(extractSealedSubtypes[A]))
      else None

    private def extractJavaEnumInstances[A: Type]: List[(String, ?<[A])] =
      Type[A].tpe.companion.decls
        .filter(_.isJavaEnum)
        .map(termSymbol => termSymbol.name.toString -> fromUntyped[A](termSymbol.asTerm.typeSignature).as_?<[A])
        .toList

    private def extractSealedSubtypes[A: Type]: List[(String, ?<[A])] = {
      def extractRecursively(t: TypeSymbol): List[TypeSymbol] =
        if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toList.map(_.asType).flatMap(extractRecursively)
        else List(t)

      // calling .distinct here as `knownDirectSubclasses` returns duplicates for multiply-inherited types
      extractRecursively(Type[A].tpe.typeSymbol.asType).distinct.map(typeSymbol =>
        subtypeName(typeSymbol) -> subtypeTypeOf[A](typeSymbol)
      ).sortBy(_._1)
    }

    private def symbolsToEnum[A: Type](subtypes: List[(String, ?<[A])]): Enum[A] =
      Enum(
        subtypes
          .map { case (name, subtypeA: ?<[A]) =>
            subtypeA.mapK[Enum.Element[A, *]] { implicit Subtype: Type[subtypeA.Underlying] => _ =>
              Enum.Element(name = name, upcast = _.upcastToExprOf[A])
            }
          }
          // with GADT we can have subtypes that shouldn't appear in pattern matching
          .filter(_.Underlying <:< Type[A])
      )

    private def subtypeName(subtype: TypeSymbol): String = subtype.name.toString
  }
}
