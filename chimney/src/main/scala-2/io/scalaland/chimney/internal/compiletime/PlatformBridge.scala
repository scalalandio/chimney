package io.scalaland.chimney.internal.compiletime

/** Scala 2 entrypoint of the macro cake: concrete macro bundles extend this class. It also hosts the Scala 2 overrides
  * of the compat workarounds (see [[MacroCommonsCompat]]).
  */
abstract private[compiletime] class PlatformBridge(val c: scala.reflect.macros.blackbox.Context)
    extends hearth.MacroCommonsScala2
    with ChimneyDefinitions {

  /** On Scala 2 the shared implementation would leave the wildcard example's existentially-quantified symbols unbound
    * after re-application - re-quantify them here (see [[MacroCommonsCompat.reapplyLeadingTypeArgsCompat]],
    * hearth#312).
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

  /** Scala 2 override of [[MacroCommonsCompat.fixJavaEnumCompat]]: decodes `runtime.RefinedJavaEnum[E, "Name"]` markers
    * (created by the Scala 2 whitebox DSL macros) back into the Java enum instance's real type.
    */
  override protected def fixJavaEnumCompat(inst: ??): ?? = {
    import c.universe.*
    val instTpe = inst.Underlying.tpe.dealias
    val refinedJavaEnumSym = symbolOf[io.scalaland.chimney.internal.runtime.RefinedJavaEnum[?, ?]]
    if (instTpe.typeConstructor.typeSymbol == refinedJavaEnumSym) {
      val javaEnum = instTpe.typeArgs.head
      val instanceName = instTpe.typeArgs(1).dealias match {
        case ConstantType(Constant(value: String)) => value
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        case other => reportError(s"Invalid RefinedJavaEnum instance name: $other!")
        // $COVERAGE-ON$
      }

      javaEnum.companion.decls
        .filter(_.isJavaEnum)
        .collectFirst {
          case sym if sym.name.decodedName.toString == instanceName =>
            c.WeakTypeTag[Any](sym.asTerm.typeSignature).as_??
        }
        .getOrElse {
          // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
          reportError("Failed at encoding Java Enum instance type")
          // $COVERAGE-ON$
        }
    } else inst
  }
}
