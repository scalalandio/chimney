package io.scalaland.chimney.internal.compiletime

/** Scala 2 entrypoint of the Hearth-based macro cake.
  *
  * Mirrors the old `DefinitionsPlatform`/`DerivationPlatform` split: concrete macro bundles will extend this class the
  * same way they extended `DefinitionsPlatform(c)` before. Its main purpose right now is to prove that the whole
  * `compiletime` cake composes and compiles on Scala 2.
  */
abstract private[compiletime] class PlatformBridge(val c: scala.reflect.macros.blackbox.Context)
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

  /** Scala 2 override of `SealedHierarchies.sealedSubtypesCompat`: port of the old macro-commons
    * `SealedHierarchiesPlatform#extractSealedSubtypes` (+ `extractJavaEnumInstances`).
    *
    * Hearth's `Type.directChildren` on Scala 2 returns a name-keyed `ListMap` of ALREADY-flattened subtypes, which
    * collapses same-named subtypes from different scopes (e.g. `colors4.Green` vs `colors4.Color.Green`) and loses the
    * ambiguity that Chimney must detect and report. This override preserves duplicates and the old position+name
    * ordering.
    */
  override protected def sealedSubtypesCompat[A: Type]: List[(String, ??<:[A])] = {
    import c.universe.*
    val A0: c.Type = Type[A].tpe

    if (A0.typeSymbol.isJavaEnum) {
      // Java enum values are unique within the enum - name collisions are impossible here.
      A0.companion.decls
        .filter(_.isJavaEnum)
        .map { termSymbol =>
          termSymbol.name.toString -> UntypedType.toTyped[A](termSymbol.asTerm.typeSignature).as_??<:[A]
        }
        .toList
    } else {
      // Workaround for <https://issues.scala-lang.org/browse/SI-7755>
      val _ = A0.typeSymbol.typeSignature

      implicit val order: Ordering[TypeSymbol] = {
        val o1 = Ordering
          .fromLessThan[c.universe.Position]((a, b) => a.line < b.line || (a.line == b.line && a.column < b.column))
          .on[TypeSymbol](_.pos)
        // Ensure parity with Scala 3 (which works around https://github.com/scala/scala3/issues/21672 bug)
        val o2 = Ordering[String].on[TypeSymbol](_.name.toString)
        (a, b) => {
          val result = o1.compare(a, b)
          if (result != 0) result else o2.compare(a, b)
        }
      }

      def extractRecursively(t: TypeSymbol): List[TypeSymbol] =
        if (t.asClass.isSealed) t.asClass.knownDirectSubclasses.toList.map(_.asType).flatMap(extractRecursively)
        else List(t)

      /** Applies type arguments from supertype to subtype if there are any (old `subtypeTypeOf`). */
      def subtypeTypeOf(subtype: TypeSymbol): c.Type = {
        val _ = subtype.typeSignature // force initialization (SI-7755)
        val sEta = subtype.toType.etaExpand
        sEta.finalResultType.substituteTypes(
          sEta.baseType(A0.typeSymbol).typeArgs.map(_.typeSymbol),
          A0.typeArgs
        )
      }

      // calling .distinct here as `knownDirectSubclasses` returns duplicates for multiply-inherited types
      extractRecursively(A0.typeSymbol.asType).distinct.sorted
        .map(typeSymbol => typeSymbol.name.toString -> UntypedType.toTyped[A](subtypeTypeOf(typeSymbol)).as_??<:[A])
    }
  }

  /** Scala 2 override of [[MacroCommonsCompat.fixJavaEnumCompat]]: port of the old
    * `ChimneyType.platformSpecific.fixJavaEnum` - decodes `runtime.RefinedJavaEnum[E, "Name"]` markers (created by the
    * Scala 2 whitebox DSL macros) back into the Java enum instance's real type.
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

  /** Scala 2 override of [[MacroCommonsCompat.isEnumCaseValCompat]]: port of the old macro-commons Scala 2 `isCaseVal`
    * formula (`isPublic && isModuleClass && isStatic && isFinal` - "parameterless case in S3 cannot be checked for
    * 'case'").
    *
    * Needed for the SANDWICH scenario: under `-Ytasty-reader` a Scala 3 parameterless enum case (e.g. `Foo.A` of
    * `enum Foo { case A }`) is seen by scalac as a static final module class WITHOUT the `Case` flag, so Hearth's
    * `Type.isCaseVal` is `false` - the type then parsed as a POJO and the engine emitted uncompilable
    * `new Foo.A.type()` instead of referencing the singleton. (Like in the old engine, this also classifies plain
    * static final `object`s as singleton "case vals" - intentional parity.)
    */
  override protected def isEnumCaseValCompat[A: Type]: Boolean = {
    val sym = Type[A].tpe.typeSymbol
    sym.isPublic && sym.isModuleClass && sym.isStatic && sym.isFinal
  }

  /** Scala 2 override of [[MacroCommonsCompat.retagExprCompat]]: re-wraps the tree with the precise `WeakTypeTag`
    * (hearth's `Type[A]` IS `c.WeakTypeTag[A]` on Scala 2), replacing the unresolved tag materialized by
    * `ValDefs.closeScope[A]` (no `Type` bound in Hearth 0.4.0).
    */
  override protected def retagExprCompat[A: Type](expr: Expr[A]): Expr[A] =
    c.Expr[A](expr.tree)(Type[A])

  /** macro-commons `Expr.nowarn` (Scala 2) - Hearth has no annotation-attaching API, so the old quasiquote-based
    * implementation lives here (see [[MacroCommonsCompat.nowarnExpr]]).
    */
  override protected def nowarnExpr[A: Type](warnings: Option[String])(expr: Expr[A]): Expr[A] = {
    import c.universe.*
    val name = c.internal.reificationSupport.freshTermName("nowarnresult$macro$")
    c.Expr[A](
      warnings.fold(
        q"""
        @ _root_.scala.annotation.nowarn
        val $name = $expr
        $name
        """
      ) { msg =>
        q"""
        @ _root_.scala.annotation.nowarn($msg)
        val $name = $expr
        $name
        """
      }
    )
  }

  /** macro-commons `Expr.SuppressWarnings` (Scala 2) - see [[nowarnExpr]]. */
  override protected def suppressWarningsExpr[A: Type](warnings: List[String])(expr: Expr[A]): Expr[A] = {
    import c.universe.*
    val name = c.internal.reificationSupport.freshTermName("suppresswarningsresult$macro$")
    c.Expr[A](
      q"""
      @ _root_.java.lang.SuppressWarnings(_root_.scala.Array(..$warnings))
      val $name = $expr
      $name
      """
    )
  }
}
