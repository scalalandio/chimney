package io.scalaland.chimney.internal.compiletime2

/** Scala 2 entrypoint of the Hearth-based macro cake.
  *
  * Mirrors the old `DefinitionsPlatform`/`DerivationPlatform` split: concrete macro bundles will extend this class the
  * same way they extended `DefinitionsPlatform(c)` before. Its main purpose right now is to prove that the whole
  * `compiletime2` cake composes and compiles on Scala 2.
  */
private[compiletime2] abstract class PlatformBridge(val c: scala.reflect.macros.blackbox.Context)
    extends hearth.MacroCommonsScala2
    with ChimneyDefinitions {

  /** On Scala 2 the shared implementation would leave the wildcard example's existentially-quantified symbols unbound
    * after re-application - re-quantify them here (see [[MacroCommonsCompat.reapplyLeadingTypeArgsCompat]]).
    */
  override protected def reapplyLeadingTypeArgsCompat(
      wildcardExample: UntypedType,
      leading: List[UntypedType]
  ): UntypedType = {
    import c.universe.*
    val dealiased = wildcardExample.dealias
    val (quantified, underlying) = dealiased match {
      case ExistentialType(qs, u) => (qs, u)
      case other                  => (Nil, other)
    }
    val applied = appliedType(underlying.typeConstructor, leading ++ underlying.typeArgs.drop(leading.size))
    internal.existentialAbstraction(quantified, applied)
  }
}
