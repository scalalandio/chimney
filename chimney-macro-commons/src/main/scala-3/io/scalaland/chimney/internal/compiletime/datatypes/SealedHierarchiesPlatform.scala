package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

trait SealedHierarchiesPlatform extends SealedHierarchies { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*
  import Type.platformSpecific.*

  protected object SealedHierarchy extends SealedHierarchyModule {

    def isJavaEnum[A: Type]: Boolean =
      TypeRepr.of[A] <:< TypeRepr.typeConstructorOf(classOf[java.lang.Enum[?]])

    def isSealed[A: Type]: Boolean = {
      val flags = TypeRepr.of[A].typeSymbol.flags
      flags.is(Flags.Sealed) // do NOT use flags.is(Flags.Enum) since it will also match enums cases!
    }

    def parse[A: Type]: Option[Enum[A]] =
      // no need for separate java.lang.Enum handling contrary to Scala 2
      if isSealed[A] then Some(symbolsToEnum(extractSealedSubtypes[A]))
      else None

    implicit private val order: Ordering[Symbol] = Ordering
      .Option(Ordering.fromLessThan[Position] { (a, b) =>
        a.startLine < b.startLine || (a.startLine == b.startLine && a.startColumn < b.startColumn)
      })
      .on[Symbol](_.pos.filter(pos => scala.util.Try(pos.start).isSuccess))
      // Stabilize order in case of https://github.com/scala/scala3/issues/21672 (does not solve the warnings!)
      .orElseBy(_.name)

    private def extractSealedSubtypes[A: Type]: List[(String, ?<[A])] = {
      def extractRecursively(sym: Symbol): List[Symbol] =
        if sym.flags.is(Flags.Sealed) then sym.children.flatMap(extractRecursively)
        else if sym.flags.is(Flags.Enum) then List(sym.typeRef.typeSymbol)
        else if sym.flags.is(Flags.Module) then List(sym.typeRef.typeSymbol.moduleClass)
        else List(sym)

      // calling .distinct here as `children` returns duplicates for multiply-inherited types
      extractRecursively(TypeRepr.of[A].typeSymbol).distinct.sorted
        .map(typeSymbol => subtypeName(typeSymbol) -> subtypeTypeOf[A](typeSymbol))
    }

    private def symbolsToEnum[A: Type](subtypes: List[(String, ?<[A])]): Enum[A] =
      Enum(
        subtypes
          .map { case (name, subtypeA: ?<[A]) =>
            subtypeA.mapK[Enum.Element[A, *]] { implicit Subtype: Type[subtypeA.Underlying] => _ =>
              Enum.Element[A, subtypeA.Underlying](name = name, upcast = _.upcastToExprOf[A])
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
