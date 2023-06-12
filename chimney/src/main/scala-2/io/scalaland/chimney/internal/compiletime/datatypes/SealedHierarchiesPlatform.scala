package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

private[compiletime] trait SealedHierarchiesPlatform extends SealedHierarchies { this: DefinitionsPlatform =>

  protected def parseAsSealedHierarchy[A: Type]: Option[SealedHierarchy[A]] = ???
}
