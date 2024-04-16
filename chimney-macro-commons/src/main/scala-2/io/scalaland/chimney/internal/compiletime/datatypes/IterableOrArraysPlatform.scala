package io.scalaland.chimney.internal.compiletime.datatypes

import io.scalaland.chimney.internal.compiletime.DefinitionsPlatform

trait IterableOrArraysPlatform extends IterableOrArrays { this: DefinitionsPlatform =>

  protected object IterableOrArray extends IterableOrArrayModule {

    def buildInIArraySupport[M: Type]: Option[Existential[IterableOrArray[M, *]]] = None
  }
}
