package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait SealedHierarchiesPlatform extends SealedHierarchies { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  protected object SealedHierarchy extends SealedHierarchyModule {

    def isSealed[A](A: Type[A]): Boolean = {
      val flags = TypeRepr.of(using A).typeSymbol.flags
      flags.is(Flags.Enum) || flags.is(Flags.Sealed)
    }

    type Subtype
    def parse[A: Type]: Option[Enum[A]] = if isSealed(Type[A]) then {
      val elements = extractSubclasses(TypeRepr.of[A].typeSymbol).distinct
        .map { (subtype: Symbol) =>
          val subtypeA = subtypeTypeOf[A](subtype)
          subtypeA.mapK[Enum.Element[A, *]] { implicit Subtype: Type[subtypeA.Underlying] => _ =>
            Enum.Element[A, subtypeA.Underlying](name = subtypeName(subtype), upcast = _.upcastExpr[A])
          }
        }
        // with GADT we can have subtypes that shouldn't appear in pattern matching
        .filter(_.Underlying <:< Type[A])
      Some(Enum(elements))
    } else None

    private def extractSubclasses(sym: Symbol): List[Symbol] =
      if sym.flags.is(Flags.Sealed) then sym.children.flatMap(extractSubclasses)
      else if sym.flags.is(Flags.Enum) then List(sym.typeRef.typeSymbol)
      else if sym.flags.is(Flags.Module) then List(sym.typeRef.typeSymbol.moduleClass)
      else List(sym)

    private def subtypeName(subtype: Symbol): String = {
      val n = subtype.name
      // case objects from Scala 2 has names with $ at the end (like all modules) while Scala 3's name
      // have all these suffixes like "$" or ".type" dropped. We need to align these names to allow comparing
      if n.endsWith("$") then n.substring(0, n.length - 1) else n
    }

    // TODO: send to review by Janek

    private def subtypeTypeOf[A: Type](subtype: Symbol): ExistentialType.UpperBounded[A] = {
      subtype.primaryConstructor.paramSymss match {
        // subtype takes type parameters
        case typeParamSymbols :: _ if typeParamSymbols.exists(_.isType) =>
          // we have to figure how subtypes type params map to parent type params
          val appliedTypeByParam: Map[String, TypeRepr] =
            subtype.typeRef
              .baseType(TypeRepr.of[A].typeSymbol)
              .typeArgs
              .map(_.typeSymbol.name)
              .zip(TypeRepr.of[A].typeArgs)
              .toMap
          // TODO: some better error message if child has an extra type param that doesn't come from the parent
          val typeParamReprs: List[TypeRepr] = typeParamSymbols.map(_.name).map(appliedTypeByParam)
          subtype.typeRef.appliedTo(typeParamReprs).asType.asInstanceOf[Type[A]].asExistentialUpperBounded[A]
        // subtype is monomorphic
        case _ =>
          subtype.typeRef.asType.asInstanceOf[Type[A]].asExistentialUpperBounded[A]
      }
    }
  }
}
