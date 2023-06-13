package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait SealedHierarchiesPlatform extends SealedHierarchies { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  protected object SealedHierarchy extends SealedHierarchyModule {

    def isSealed[A](A: Type[A]): Boolean = {
      val flags = TypeRepr.of(using A).typeSymbol.flags
      flags.is(Flags.Enum) || flags.is(Flags.Sealed)
    }

    def parse[A: Type]: Option[Enum[A]] = ???
  }
}
