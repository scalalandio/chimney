package io.scalaland.chimney.internal.compiletime

import scala.collection.Factory

/** Scala Native stub of the `java.util.EnumSet`/`java.util.EnumMap` compat for the Hearth-provider fallback layer.
  *
  * Hearth 0.4.0 ships its `java.util.*` providers only on the JVM, so the fallback can never match these types here -
  * detection is constantly `false` and the factory builders are unreachable. See the JVM source of the same name for
  * the real implementation and the provider bug it works around.
  */
private[compiletime] trait JavaCollectionsPlatformCompat {
  this: ChimneyDefinitions & hearth.MacroCommons =>

  protected def isJavaEnumSetCompat[M: Type]: Boolean = false

  protected def isJavaEnumMapCompat[M: Type]: Boolean = false

  protected def javaCollectionIteratorCompat[Item: Type, M: Type](collection: Expr[M]): Expr[Iterator[Item]] =
    // $COVERAGE-OFF$unreachable - the detection above never returns true on this platform
    assertionFailed(s"java.util.EnumSet is not supported on this platform (got ${Type.prettyPrint[M]})")
  // $COVERAGE-ON$

  protected def javaMapIteratorCompat[K: Type, V: Type, M: Type](collection: Expr[M]): Expr[Iterator[(K, V)]] =
    // $COVERAGE-OFF$unreachable - the detection above never returns true on this platform
    assertionFailed(s"java.util.EnumMap is not supported on this platform (got ${Type.prettyPrint[M]})")
  // $COVERAGE-ON$

  protected def javaEnumSetFactoryCompat[Item: Type, M: Type](
      itemClass: Expr[java.lang.Class[Item]]
  ): Expr[Factory[Item, M]] =
    // $COVERAGE-OFF$unreachable - the detection above never returns true on this platform
    assertionFailed(s"java.util.EnumSet is not supported on this platform (got ${Type.prettyPrint[M]})")
  // $COVERAGE-ON$

  protected def javaEnumMapFactoryCompat[K: Type, V: Type, M: Type](
      keyClass: Expr[java.lang.Class[K]]
  ): Expr[Factory[(K, V), M]] =
    // $COVERAGE-OFF$unreachable - the detection above never returns true on this platform
    assertionFailed(s"java.util.EnumMap is not supported on this platform (got ${Type.prettyPrint[M]})")
  // $COVERAGE-ON$
}
