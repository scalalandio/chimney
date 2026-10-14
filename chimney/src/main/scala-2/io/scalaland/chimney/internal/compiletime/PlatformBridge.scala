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

  /** Scala 2 override of [[MacroCommonsCompat.nowarnExpr]]: positions the annotation, see [[withAnnotationPositions]].
    */
  override protected def nowarnExpr[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A] =
    withAnnotationPositions(super.nowarnExpr(warnings)(expr))

  /** Scala 2 override of [[MacroCommonsCompat.suppressWarningsExpr]]: positions the annotation, see
    * [[withAnnotationPositions]].
    */
  override protected def suppressWarningsExpr[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A] =
    withAnnotationPositions(super.suppressWarningsExpr(warnings)(expr))

  /** `Expr.annotated` generates `{ @Ann(...) val fresh = expr; fresh }` without positions, and the macro expansion
    * positions only the trees, not the annotations in their `Modifiers`. scalac registers each `@nowarn` by its
    * annotation's position (`Position.point`), so a position-less one crashes the compiler with
    * `UnsupportedOperationException: Position.point on NoPosition` - we use the macro call-site position instead.
    */
  private def withAnnotationPositions[A: Type](expr: Expr[A]): Expr[A] = {
    import c.universe.*
    val pos = c.enclosingPosition.focus
    expr.tree match {
      case block @ Block(stats, result) =>
        val positionedStats = stats.map {
          case valDef @ ValDef(mods, name, tpt, rhs) if mods.annotations.exists(_.pos == NoPosition) =>
            val annotations = mods.annotations.map(ann => if (ann.pos == NoPosition) atPos(pos)(ann) else ann)
            treeCopy.ValDef(valDef, Modifiers(mods.flags, mods.privateWithin, annotations), name, tpt, rhs)
          case stat => stat
        }
        c.Expr[A](treeCopy.Block(block, positionedStats, result))
      // $COVERAGE-OFF$should never happen unless Expr.annotated changes its generated shape
      case _ => expr
      // $COVERAGE-ON$
    }
  }
}
