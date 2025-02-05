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

    private type Cached[A] = Option[Enum[A]]
    private val enumCache = new Type.Cache[Cached]
    def parse[A: Type]: Option[Enum[A]] = enumCache(Type[A]) {
      if (isJavaEnum[A]) Some(symbolsToEnum(extractJavaEnumInstances[A]))
      else if (isSealed[A]) Some(symbolsToEnum(extractSealedSubtypes[A]))
      else None
    }

    private def extractJavaEnumInstances[A: Type]: List[(String, ?<[A])] =
      Type[A].tpe.companion.decls
        .filter(_.isJavaEnum)
        .map(termSymbol => termSymbol.name.toString -> fromUntyped[A](termSymbol.asTerm.typeSignature).as_?<[A])
        .toList

    implicit private val order: Ordering[TypeSymbol] = {
      val o1 = Ordering
        .fromLessThan[Position]((a, b) => a.line < b.line || (a.line == b.line && a.column < b.column))
        .on[TypeSymbol](_.pos)
      // Ensure parity with Scala 3 (which workes around https://github.com/scala/scala3/issues/21672 bug)
      val o2 = Ordering[String].on[TypeSymbol](subtypeName)
      (a, b) => {
        val result = o1.compare(a, b)
        if (result != 0) result else o2.compare(a, b)
      }
    }

    private def extractSealedSubtypes[A: Type]: List[(String, ?<[A])] = {
      forceTypeSymbolInitialization[A]

      def extractRecursively(t: TypeSymbol): List[TypeSymbol] =
        if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toList.map(_.asType).flatMap(extractRecursively)
        else List(t)

      // calling .distinct here as `knownDirectSubclasses` returns duplicates for multiply-inherited types
      extractRecursively(Type[A].tpe.typeSymbol.asType).distinct.sorted
        .map(typeSymbol => subtypeName(typeSymbol) -> subtypeTypeOf[A](typeSymbol))
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
