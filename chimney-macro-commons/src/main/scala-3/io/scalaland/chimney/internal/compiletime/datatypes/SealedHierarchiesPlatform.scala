package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

trait SealedHierarchiesPlatform extends SealedHierarchies { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*
  import Type.platformSpecific.*

  protected object SealedHierarchy extends SealedHierarchyModule {

    def isSealed[A](A: Type[A]): Boolean = {
      val flags = TypeRepr.of(using A).typeSymbol.flags
      flags.is(Flags.Enum) || flags.is(Flags.Sealed)
    }

    def isJavaEnum[A](A: Type[A]): Boolean =
      TypeRepr.of(using A) <:< TypeRepr.typeConstructorOf(classOf[java.lang.Enum[?]])

    def parse[A: Type]: Option[Enum[A]] =
      if isSealed(Type[A]) then Some(symbolsToEnum(extractSubclasses(TypeRepr.of[A].typeSymbol)))
      else if isJavaEnum(Type[A]) then Some(symbolsToEnum(extractJavaEnumInstances(TypeRepr.of[A].typeSymbol)))
      else None

    private def extractSubclasses(sym: Symbol): List[Symbol] =
      if sym.flags.is(Flags.Sealed) then sym.children.flatMap(extractSubclasses)
      else if sym.flags.is(Flags.Enum) then List(sym.typeRef.typeSymbol)
      else if sym.flags.is(Flags.Module) then List(sym.typeRef.typeSymbol.moduleClass)
      else List(sym)

    private def extractJavaEnumInstances(sym: Symbol): List[Symbol] =
      sym.fieldMembers.filter { m =>
        m.typeRef <:< TypeRepr.typeConstructorOf(classOf[java.lang.Enum[?]])
      }

    private def symbolsToEnum[A: Type](subtypes: List[Symbol]): Enum[A] =
      Enum(
        // calling .distinct here as `knownDirectSubclasses` returns duplicates for multiply-inherited types
        subtypes.distinct
          .map { (subtype: Symbol) =>
            val subtypeA = subtypeTypeOf[A](subtype)
            subtypeA.mapK[Enum.Element[A, *]] { implicit Subtype: Type[subtypeA.Underlying] => _ =>
              Enum.Element[A, subtypeA.Underlying](name = subtypeName(subtype), upcast = _.upcastExpr[A])
            }
          }
          // with GADT we can have subtypes that shouldn't appear in pattern matching
          .filter(_.Underlying <:< Type[A])
      )

    private def subtypeName(subtype: Symbol): String = {
      val n = subtype.name
      // case objects from Scala 2 has names with $ at the end (like all modules) while Scala 3's name
      // have all these suffixes like "$" or ".type" dropped. We need to align these names to allow comparing
      if n.endsWith("$") then n.substring(0, n.length - 1) else n
    }
  }
}
