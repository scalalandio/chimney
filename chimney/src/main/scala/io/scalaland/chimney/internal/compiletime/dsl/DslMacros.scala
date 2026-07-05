package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal.compiletime.ChimneyDefinitions
import io.scalaland.chimney.internal.runtime

/** Shared (Scala 2 + Scala 3) bodies of all DSL macros: parse the selector/constructor lambdas, compute the refined
  * `TransformerOverrides`/`PatcherOverrides` type and re-type the DSL value to it (whitebox on Scala 2, transparent
  * inline on Scala 3 - both read the refined type off the returned tree).
  *
  * Platform adapters stay thin: Scala 2 whitebox bundles pass `c.prefix` and argument trees here, Scala 3 static
  * entrypoints instantiate the cake per call and forward the quoted arguments.
  */
private[compiletime] trait DslMacros extends DslDefinitions { this: ChimneyDefinitions & hearth.MacroCommons =>

  import ChimneyType.{PatcherOverrides as PO, TransformerOverrides as TO}

  // --- DSL wrapper type constructors (Type.CtorN.UpperBounded.of, hearth#307/#344) ---

  private lazy val TransformerDefinitionT = Type.Ctor4.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    dsls.TransformerDefinition
  ]

  private lazy val TransformerIntoT = Type.Ctor4.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    dsls.TransformerInto
  ]

  private lazy val PartialTransformerDefinitionT = Type.Ctor4.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    dsls.PartialTransformerDefinition
  ]

  private lazy val PartialTransformerIntoT = Type.Ctor4.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    dsls.PartialTransformerInto
  ]

  private lazy val PatcherDefinitionT = Type.Ctor4.UpperBounded.of[
    Any,
    Any,
    runtime.PatcherOverrides,
    runtime.PatcherFlags,
    dsls.PatcherDefinition
  ]

  private lazy val PatcherUsingT = Type.Ctor4.UpperBounded.of[
    Any,
    Any,
    runtime.PatcherOverrides,
    runtime.PatcherFlags,
    dsls.PatcherUsing
  ]

  private lazy val CodecDefinitionT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    dsls.CodecDefinition
  ]

  private lazy val IsoDefinitionT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    dsls.IsoDefinition
  ]

  // ChimneyTypes' ForAll module is unapply-only (the engine never constructs it) - the DSL needs the apply.
  private lazy val ForAllT = Type.Ctor4.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerOverrides,
    runtime.TransformerOverrides.ForAll
  ]

  // Flags-DSL wrapper ctors: `Type.of[X[...]]` with BOUNDED method type params loses the bounds in the Scala 2
  // cross-quotes expansion ("type arguments do not conform..."), so these also go through the compat factories.
  private lazy val SourceFlagsOfTransformerDefinitionT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    runtime.Path,
    dsls.TransformerSourceFlagsDsl.OfTransformerDefinition
  ]

  private lazy val TargetFlagsOfTransformerDefinitionT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    runtime.Path,
    dsls.TransformerTargetFlagsDsl.OfTransformerDefinition
  ]

  private lazy val SourceFlagsOfTransformerIntoT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    runtime.Path,
    dsls.TransformerSourceFlagsDsl.OfTransformerInto
  ]

  private lazy val TargetFlagsOfTransformerIntoT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    runtime.Path,
    dsls.TransformerTargetFlagsDsl.OfTransformerInto
  ]

  private lazy val SourceFlagsOfPartialTransformerDefinitionT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    runtime.Path,
    dsls.TransformerSourceFlagsDsl.OfPartialTransformerDefinition
  ]

  private lazy val TargetFlagsOfPartialTransformerDefinitionT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    runtime.Path,
    dsls.TransformerTargetFlagsDsl.OfPartialTransformerDefinition
  ]

  private lazy val SourceFlagsOfPartialTransformerIntoT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    runtime.Path,
    dsls.TransformerSourceFlagsDsl.OfPartialTransformerInto
  ]

  private lazy val TargetFlagsOfPartialTransformerIntoT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.TransformerOverrides,
    runtime.TransformerFlags,
    runtime.Path,
    dsls.TransformerTargetFlagsDsl.OfPartialTransformerInto
  ]

  private lazy val PatchedValueFlagsOfPatcherDefinitionT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.PatcherOverrides,
    runtime.PatcherFlags,
    runtime.Path,
    dsls.PatcherPatchedValueFlagsDsl.OfPatcherDefinition
  ]

  private lazy val PatchedValueFlagsOfPatcherUsingT = Type.Ctor5.UpperBounded.of[
    Any,
    Any,
    runtime.PatcherOverrides,
    runtime.PatcherFlags,
    runtime.Path,
    dsls.PatcherPatchedValueFlagsDsl.OfPatcherUsing
  ]

  private lazy val WithRuntimeDataStoreT: Type[runtime.WithRuntimeDataStore] = Type.of[runtime.WithRuntimeDataStore]

  // --- refined TransformerOverrides/PatcherOverrides building ---

  object DslOverride {

    def const[Overrides <: runtime.TransformerOverrides: Type](
        toPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import toPath.Underlying as ToPath
      TO.Const[ToPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def constPartial[Overrides <: runtime.TransformerOverrides: Type](
        toPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import toPath.Underlying as ToPath
      TO.ConstPartial[ToPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def computed[Overrides <: runtime.TransformerOverrides: Type](
        fromPath: ??<:[runtime.Path],
        toPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import fromPath.Underlying as FromPath, toPath.Underlying as ToPath
      TO.Computed[FromPath, ToPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def computedPartial[Overrides <: runtime.TransformerOverrides: Type](
        fromPath: ??<:[runtime.Path],
        toPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import fromPath.Underlying as FromPath, toPath.Underlying as ToPath
      TO.ComputedPartial[FromPath, ToPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def computedPartialFailFast[Overrides <: runtime.TransformerOverrides: Type](
        fromPath: ??<:[runtime.Path],
        toPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import fromPath.Underlying as FromPath, toPath.Underlying as ToPath
      TO.ComputedPartialFailFast[FromPath, ToPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def renamed[Overrides <: runtime.TransformerOverrides: Type](
        fromPath: ??<:[runtime.Path],
        toPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import fromPath.Underlying as FromPath, toPath.Underlying as ToPath
      TO.Renamed[FromPath, ToPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def unused[Overrides <: runtime.TransformerOverrides: Type](
        fromPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import fromPath.Underlying as FromPath
      TO.Unused[FromPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def unmatched[Overrides <: runtime.TransformerOverrides: Type](
        toPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import toPath.Underlying as ToPath
      TO.Unmatched[ToPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def fallback[Overrides <: runtime.TransformerOverrides: Type](
        fromFallback: ??,
        fromPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import fromFallback.Underlying as FromFallback, fromPath.Underlying as FromPath
      TO.Fallback[FromFallback, FromPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def constructor[Overrides <: runtime.TransformerOverrides: Type](
        args: ??<:[runtime.ArgumentLists],
        toPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import args.Underlying as Args, toPath.Underlying as ToPath
      TO.Constructor[Args, ToPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def constructorPartial[Overrides <: runtime.TransformerOverrides: Type](
        args: ??<:[runtime.ArgumentLists],
        toPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import args.Underlying as Args, toPath.Underlying as ToPath
      TO.ConstructorPartial[Args, ToPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def constructorPartialFailFast[Overrides <: runtime.TransformerOverrides: Type](
        args: ??<:[runtime.ArgumentLists],
        toPath: ??<:[runtime.Path]
    ): ??<:[runtime.TransformerOverrides] = {
      import args.Underlying as Args, toPath.Underlying as ToPath
      TO.ConstructorPartialFailFast[Args, ToPath, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def forAll[FromMatch: Type, ToMatch: Type, Overrides <: runtime.TransformerOverrides: Type](
        inner: ??<:[runtime.TransformerOverrides]
    ): ??<:[runtime.TransformerOverrides] = {
      import inner.Underlying as Inner
      ForAllT[FromMatch, ToMatch, Inner, Overrides].as_??<:[runtime.TransformerOverrides]
    }

    def patcherConst[Overrides <: runtime.PatcherOverrides: Type](
        objPath: ??<:[runtime.Path]
    ): ??<:[runtime.PatcherOverrides] = {
      import objPath.Underlying as ObjPath
      PO.Const[ObjPath, Overrides].as_??<:[runtime.PatcherOverrides]
    }

    def patcherComputed[Overrides <: runtime.PatcherOverrides: Type](
        patchPath: ??<:[runtime.Path],
        objPath: ??<:[runtime.Path]
    ): ??<:[runtime.PatcherOverrides] = {
      import patchPath.Underlying as PatchPath, objPath.Underlying as ObjPath
      PO.Computed[PatchPath, ObjPath, Overrides].as_??<:[runtime.PatcherOverrides]
    }

    def patcherIgnored[Overrides <: runtime.PatcherOverrides: Type](
        patchPath: ??<:[runtime.Path]
    ): ??<:[runtime.PatcherOverrides] = {
      import patchPath.Underlying as PatchPath
      PO.Ignored[PatchPath, Overrides].as_??<:[runtime.PatcherOverrides]
    }
  }

  // --- cross-quotes helpers (hoisted to trait level; fully-qualified names inside quotes) ---

  private def updatedAndCastedTo[B: Type](
      instance: Expr[runtime.WithRuntimeDataStore],
      data: Expr[Any]
  ): Expr[B] = Expr.quote {
    io.scalaland.chimney.internal.runtime.WithRuntimeDataStore
      .update(Expr.splice(instance), Expr.splice(data))
      .asInstanceOf[B]
  }

  /** Curries `(A, Boolean) => B` fail-fast overrides into the `A => Boolean => B` shape expected by the runtime data
    * store; `A`/`B` are recovered from the function expression's type.
    */
  def curriedFailFastData(f: Expr[Any]): Expr[Any] = {
    val fType: Expr_?? = UntypedExpr.as_??(UntypedExpr.fromTyped(f))
    UntypedType.typeArguments(UntypedType.dealias(fType.Underlying.asUntyped)) match {
      case s :: _ :: r :: Nil =>
        val sType: ?? = s.asTyped[Any].as_??
        val rType: ?? = r.asTyped[Any].as_??
        import sType.Underlying as S, rType.Underlying as R
        curriedFailFast[S, R](f.asInstanceOf[Expr[(S, Boolean) => R]]).asInstanceOf[Expr[Any]]
      case _ =>
        // $COVERAGE-OFF$the DSL signatures guarantee a Function2
        assertionFailed(s"Expected a two-argument function, got ${Type.prettyPrint(using fType.Underlying)}")
      // $COVERAGE-ON$
    }
  }

  private def curriedFailFast[S: Type, R: Type](f: Expr[(S, Boolean) => R]): Expr[S => Boolean => R] = Expr.quote {
    val fn = Expr.splice(f)
    (s: S) => (failFast: Boolean) => fn(s, failFast)
  }

  /** Lifts an `Either`-returning override function into a `partial.Result`-returning one; the `FunctionEitherToResult`
    * instance is summoned for the function expression's own type.
    */
  def liftedEitherData(f: Expr[Any]): Expr[Any] = {
    val fType: Expr_?? = UntypedExpr.as_??(UntypedExpr.fromTyped(f))
    import fType.Underlying as Ctor
    liftedEither[Ctor](f.asInstanceOf[Expr[Ctor]])
  }

  private def liftedEither[Ctor: Type](f: Expr[Ctor]): Expr[Any] = {
    val ev =
      summonImplicitUnsafeOf[runtime.FunctionEitherToResult[Ctor]](using Type.of[runtime.FunctionEitherToResult[Ctor]])
    Expr.quote {
      io.scalaland.chimney.internal.runtime.FunctionEitherToResult.lift[Ctor, Any](Expr.splice(f))(
        Expr
          .splice(ev)
          .asInstanceOf[io.scalaland.chimney.internal.runtime.FunctionEitherToResult.Aux[Ctor, Any]]
      )
    }
  }

  /** `Expr_??` -> platform tree, for the Scala 2 whitebox adapters. */
  implicit final class DslResultOps(private val result: Expr_??) {

    def toUntypedResult: UntypedExpr = UntypedExpr.fromTyped(result.value)
  }

  // --- shared macro bodies ---

  /** Bodies shared by `TransformerDefinition`/`TransformerInto`/`PartialTransformerDefinition`/`PartialTransformerInto`
    * macros (the `W`-independent parts; total wrappers simply never call the `*Partial*` entries).
    */
  final class TransformerDslBodies[W[_, _, _ <: runtime.TransformerOverrides, _ <: runtime.TransformerFlags]](
      W: Type.Ctor4.UpperBounded[Any, Any, runtime.TransformerOverrides, runtime.TransformerFlags, W]
  ) {

    private def updated[From: Type, To: Type, Flags <: runtime.TransformerFlags: Type](
        prefix: Expr[runtime.WithRuntimeDataStore],
        data: Expr[Any],
        overrideType: ??<:[runtime.TransformerOverrides]
    ): Expr_?? = {
      import overrideType.Underlying as O2
      implicit val ResultType: Type[W[From, To, O2, Flags]] = W[From, To, O2, Flags]
      updatedAndCastedTo[W[From, To, O2, Flags]](prefix, data).as_??
    }

    private def casted[From: Type, To: Type, Flags <: runtime.TransformerFlags: Type](
        prefix: Expr[runtime.WithRuntimeDataStore],
        overrideType: ??<:[runtime.TransformerOverrides]
    ): Expr_?? = {
      import overrideType.Underlying as O2
      implicit val ResultType: Type[W[From, To, O2, Flags]] = W[From, To, O2, Flags]
      castToExpr[runtime.WithRuntimeDataStore, W[From, To, O2, Flags]](prefix)(using
        WithRuntimeDataStoreT,
        ResultType
      ).as_??
    }

    def withFieldConst[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selector: Expr[Any], value: Expr[Any]): Expr_?? =
      withPathType(selector)(toPath => updated[From, To, Flags](prefix, value, DslOverride.const[Overrides](toPath)))

    def withFieldConstPartial[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selector: Expr[Any], value: Expr[Any]): Expr_?? =
      withPathType(selector)(toPath =>
        updated[From, To, Flags](prefix, value, DslOverride.constPartial[Overrides](toPath))
      )

    def withFieldComputed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selector: Expr[Any], f: Expr[Any]): Expr_?? =
      withPathType(selector)(toPath =>
        updated[From, To, Flags](prefix, f, DslOverride.computed[Overrides](rootPathType, toPath))
      )

    def withFieldComputedFrom[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](
        prefix: Expr[runtime.WithRuntimeDataStore],
        selectorFrom: Expr[Any],
        selectorTo: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathTypes(selectorFrom, selectorTo)((fromPath, toPath) =>
        updated[From, To, Flags](prefix, f, DslOverride.computed[Overrides](fromPath, toPath))
      )

    def withFieldComputedPartial[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selector: Expr[Any], f: Expr[Any]): Expr_?? =
      withPathType(selector)(toPath =>
        updated[From, To, Flags](prefix, f, DslOverride.computedPartial[Overrides](rootPathType, toPath))
      )

    def withFieldComputedPartialFrom[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](
        prefix: Expr[runtime.WithRuntimeDataStore],
        selectorFrom: Expr[Any],
        selectorTo: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathTypes(selectorFrom, selectorTo)((fromPath, toPath) =>
        updated[From, To, Flags](prefix, f, DslOverride.computedPartial[Overrides](fromPath, toPath))
      )

    def withFieldComputedPartialFailFast[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selector: Expr[Any], f: Expr[Any]): Expr_?? =
      withPathType(selector)(toPath =>
        updated[From, To, Flags](
          prefix,
          curriedFailFastData(f),
          DslOverride.computedPartialFailFast[Overrides](rootPathType, toPath)
        )
      )

    def withFieldComputedPartialFromFailFast[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](
        prefix: Expr[runtime.WithRuntimeDataStore],
        selectorFrom: Expr[Any],
        selectorTo: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathTypes(selectorFrom, selectorTo)((fromPath, toPath) =>
        updated[From, To, Flags](
          prefix,
          curriedFailFastData(f),
          DslOverride.computedPartialFailFast[Overrides](fromPath, toPath)
        )
      )

    def withFieldRenamed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selectorFrom: Expr[Any], selectorTo: Expr[Any]): Expr_?? =
      withPathTypes(selectorFrom, selectorTo)((fromPath, toPath) =>
        casted[From, To, Flags](prefix, DslOverride.renamed[Overrides](fromPath, toPath))
      )

    def withFieldUnused[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selectorFrom: Expr[Any]): Expr_?? =
      withPathType(selectorFrom)(fromPath => casted[From, To, Flags](prefix, DslOverride.unused[Overrides](fromPath)))

    def withSealedSubtypeHandled[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], f: Expr[Any], subtype: ??): Expr_?? =
      updated[From, To, Flags](
        prefix,
        f,
        DslOverride.computed[Overrides](sourceMatchingRootPath(javaEnumFixedSubtype(f, subtype)), rootPathType)
      )

    def withSealedSubtypeHandledPartial[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], f: Expr[Any], subtype: ??): Expr_?? =
      updated[From, To, Flags](
        prefix,
        f,
        DslOverride.computedPartial[Overrides](sourceMatchingRootPath(javaEnumFixedSubtype(f, subtype)), rootPathType)
      )

    def withSealedSubtypeHandledPartialFailFast[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], f: Expr[Any], subtype: ??): Expr_?? =
      updated[From, To, Flags](
        prefix,
        curriedFailFastData(f),
        DslOverride
          .computedPartialFailFast[Overrides](sourceMatchingRootPath(javaEnumFixedSubtype(f, subtype)), rootPathType)
      )

    def withSealedSubtypeRenamed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], fromSubtype: ??, toSubtype: ??): Expr_?? =
      casted[From, To, Flags](
        prefix,
        DslOverride.renamed[Overrides](sourceMatchingRootPath(fromSubtype), matchingRootPath(toSubtype))
      )

    def withSealedSubtypeUnmatched[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selectorTo: Expr[Any]): Expr_?? =
      withPathType(selectorTo)(toPath => casted[From, To, Flags](prefix, DslOverride.unmatched[Overrides](toPath)))

    def withFallback[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], fromFallback: ??, fallback: Expr[Any]): Expr_?? =
      updated[From, To, Flags](prefix, fallback, DslOverride.fallback[Overrides](fromFallback, rootPathType))

    def withFallbackFrom[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](
        prefix: Expr[runtime.WithRuntimeDataStore],
        fromFallback: ??,
        selectorFrom: Expr[Any],
        fallback: Expr[Any]
    ): Expr_?? =
      withPathType(selectorFrom)(fromPath =>
        updated[From, To, Flags](prefix, fallback, DslOverride.fallback[Overrides](fromFallback, fromPath))
      )

    def withConstructor[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], f: Expr[Any]): Expr_?? =
      withArgumentListsType(f)(args =>
        updated[From, To, Flags](prefix, f, DslOverride.constructor[Overrides](args, rootPathType))
      )

    def withConstructorTo[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selector: Expr[Any], f: Expr[Any]): Expr_?? =
      withArgumentListsType(f)(args =>
        withPathType(selector)(toPath =>
          updated[From, To, Flags](prefix, f, DslOverride.constructor[Overrides](args, toPath))
        )
      )

    def withConstructorPartial[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], f: Expr[Any]): Expr_?? =
      withArgumentListsType(f)(args =>
        updated[From, To, Flags](prefix, f, DslOverride.constructorPartial[Overrides](args, rootPathType))
      )

    def withConstructorPartialTo[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selector: Expr[Any], f: Expr[Any]): Expr_?? =
      withArgumentListsType(f)(args =>
        withPathType(selector)(toPath =>
          updated[From, To, Flags](prefix, f, DslOverride.constructorPartial[Overrides](args, toPath))
        )
      )

    def withConstructorPartialFailFast[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], f: Expr[Any]): Expr_?? =
      withArgumentListsType(f)(args =>
        updated[From, To, Flags](prefix, f, DslOverride.constructorPartialFailFast[Overrides](args, rootPathType))
      )

    def withConstructorPartialToFailFast[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selector: Expr[Any], f: Expr[Any]): Expr_?? =
      withArgumentListsType(f)(args =>
        withPathType(selector)(toPath =>
          updated[From, To, Flags](prefix, f, DslOverride.constructorPartialFailFast[Overrides](args, toPath))
        )
      )

    def withConstructorEither[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], f: Expr[Any]): Expr_?? =
      withArgumentListsType(f)(args =>
        updated[From, To, Flags](
          prefix,
          liftedEitherData(f),
          DslOverride.constructorPartial[Overrides](args, rootPathType)
        )
      )

    def withConstructorEitherTo[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selector: Expr[Any], f: Expr[Any]): Expr_?? =
      withArgumentListsType(f)(args =>
        withPathType(selector)(toPath =>
          updated[From, To, Flags](prefix, liftedEitherData(f), DslOverride.constructorPartial[Overrides](args, toPath))
        )
      )
  }

  lazy val TransformerDefinitionDsl: TransformerDslBodies[dsls.TransformerDefinition] =
    new TransformerDslBodies(TransformerDefinitionT)
  lazy val TransformerIntoDsl: TransformerDslBodies[dsls.TransformerInto] =
    new TransformerDslBodies(TransformerIntoT)
  lazy val PartialTransformerDefinitionDsl: TransformerDslBodies[dsls.PartialTransformerDefinition] =
    new TransformerDslBodies(PartialTransformerDefinitionT)
  lazy val PartialTransformerIntoDsl: TransformerDslBodies[dsls.PartialTransformerInto] =
    new TransformerDslBodies(PartialTransformerIntoT)

  /** Bodies shared by `PatcherDefinition`/`PatcherUsing` macros. */
  final class PatcherDslBodies[W[_, _, _ <: runtime.PatcherOverrides, _ <: runtime.PatcherFlags]](
      W: Type.Ctor4.UpperBounded[Any, Any, runtime.PatcherOverrides, runtime.PatcherFlags, W]
  ) {

    private def updated[A: Type, Patch: Type, Flags <: runtime.PatcherFlags: Type](
        prefix: Expr[runtime.WithRuntimeDataStore],
        data: Expr[Any],
        overrideType: ??<:[runtime.PatcherOverrides]
    ): Expr_?? = {
      import overrideType.Underlying as O2
      implicit val ResultType: Type[W[A, Patch, O2, Flags]] = W[A, Patch, O2, Flags]
      updatedAndCastedTo[W[A, Patch, O2, Flags]](prefix, data).as_??
    }

    private def casted[A: Type, Patch: Type, Flags <: runtime.PatcherFlags: Type](
        prefix: Expr[runtime.WithRuntimeDataStore],
        overrideType: ??<:[runtime.PatcherOverrides]
    ): Expr_?? = {
      import overrideType.Underlying as O2
      implicit val ResultType: Type[W[A, Patch, O2, Flags]] = W[A, Patch, O2, Flags]
      castToExpr[runtime.WithRuntimeDataStore, W[A, Patch, O2, Flags]](prefix)(using
        WithRuntimeDataStoreT,
        ResultType
      ).as_??
    }

    def withFieldConst[
        A: Type,
        Patch: Type,
        Overrides <: runtime.PatcherOverrides: Type,
        Flags <: runtime.PatcherFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selectorObj: Expr[Any], value: Expr[Any]): Expr_?? =
      withPathType(selectorObj)(objPath =>
        updated[A, Patch, Flags](prefix, value, DslOverride.patcherConst[Overrides](objPath))
      )

    def withFieldComputed[
        A: Type,
        Patch: Type,
        Overrides <: runtime.PatcherOverrides: Type,
        Flags <: runtime.PatcherFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selectorObj: Expr[Any], f: Expr[Any]): Expr_?? =
      withPathType(selectorObj)(objPath =>
        updated[A, Patch, Flags](prefix, f, DslOverride.patcherComputed[Overrides](rootPathType, objPath))
      )

    def withFieldComputedFrom[
        A: Type,
        Patch: Type,
        Overrides <: runtime.PatcherOverrides: Type,
        Flags <: runtime.PatcherFlags: Type
    ](
        prefix: Expr[runtime.WithRuntimeDataStore],
        selectorPatch: Expr[Any],
        selectorObj: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathTypes(selectorPatch, selectorObj)((patchPath, objPath) =>
        updated[A, Patch, Flags](prefix, f, DslOverride.patcherComputed[Overrides](patchPath, objPath))
      )

    def withFieldIgnored[
        A: Type,
        Patch: Type,
        Overrides <: runtime.PatcherOverrides: Type,
        Flags <: runtime.PatcherFlags: Type
    ](prefix: Expr[runtime.WithRuntimeDataStore], selectorPatch: Expr[Any]): Expr_?? =
      withPathType(selectorPatch)(patchPath =>
        casted[A, Patch, Flags](prefix, DslOverride.patcherIgnored[Overrides](patchPath))
      )
  }

  lazy val PatcherDefinitionDsl: PatcherDslBodies[dsls.PatcherDefinition] = new PatcherDslBodies(PatcherDefinitionT)
  lazy val PatcherUsingDsl: PatcherDslBodies[dsls.PatcherUsing] = new PatcherDslBodies(PatcherUsingT)

  /** Bodies shared by `CodecDefinition`/`IsoDefinition` macros (a pair of mirrored `Renamed` overrides). */
  final class IsoLikeDslBodies[
      W[_, _, _ <: runtime.TransformerOverrides, _ <: runtime.TransformerOverrides, _ <: runtime.TransformerFlags]
  ](
      W: Type.Ctor5.UpperBounded[
        Any,
        Any,
        runtime.TransformerOverrides,
        runtime.TransformerOverrides,
        runtime.TransformerFlags,
        W
      ]
  ) {

    private def casted[A: Type, B: Type, Flags <: runtime.TransformerFlags: Type](
        prefix: Expr[Any],
        firstOverride: ??<:[runtime.TransformerOverrides],
        secondOverride: ??<:[runtime.TransformerOverrides]
    ): Expr_?? = {
      import firstOverride.Underlying as O1, secondOverride.Underlying as O2
      implicit val AnyType: Type[Any] = Type.of[Any]
      implicit val ResultType: Type[W[A, B, O1, O2, Flags]] = W[A, B, O1, O2, Flags]
      castToExpr[Any, W[A, B, O1, O2, Flags]](prefix).as_??
    }

    def withFieldRenamed[
        A: Type,
        B: Type,
        FirstOverrides <: runtime.TransformerOverrides: Type,
        SecondOverrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[Any], selectorFirst: Expr[Any], selectorSecond: Expr[Any]): Expr_?? =
      withPathTypes(selectorFirst, selectorSecond)((firstPath, secondPath) =>
        casted[A, B, Flags](
          prefix,
          DslOverride.renamed[FirstOverrides](firstPath, secondPath),
          DslOverride.renamed[SecondOverrides](secondPath, firstPath)
        )
      )

    def withSealedSubtypeRenamed[
        A: Type,
        B: Type,
        FirstOverrides <: runtime.TransformerOverrides: Type,
        SecondOverrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type
    ](prefix: Expr[Any], firstSubtype: ??, secondSubtype: ??): Expr_?? =
      casted[A, B, Flags](
        prefix,
        DslOverride.renamed[FirstOverrides](
          sourceMatchingRootPath(firstSubtype),
          matchingRootPath(secondSubtype)
        ),
        DslOverride.renamed[SecondOverrides](
          sourceMatchingRootPath(secondSubtype),
          matchingRootPath(firstSubtype)
        )
      )
  }

  lazy val CodecDefinitionDsl: IsoLikeDslBodies[dsls.CodecDefinition] = new IsoLikeDslBodies(CodecDefinitionT)
  lazy val IsoDefinitionDsl: IsoLikeDslBodies[dsls.IsoDefinition] = new IsoLikeDslBodies(IsoDefinitionT)

  // --- ForAll bodies (construct new DSL wrappers rather than casting the prefix) ---

  object TransformerDefinitionForAllDsl {

    private def make[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        data: Option[Expr[Any]],
        inner: ??<:[runtime.TransformerOverrides]
    ): Expr_?? = {
      val forAllType: ??<:[runtime.TransformerOverrides] = DslOverride.forAll[FromMatch, ToMatch, Overrides](inner)
      import forAllType.Underlying as O2
      implicit val ResultType: Type[dsls.TransformerDefinition[From, To, O2, Flags]] =
        TransformerDefinitionT[From, To, O2, Flags]
      data
        .fold(
          Expr.quote {
            new io.scalaland.chimney.dsl.TransformerDefinition[From, To, O2, Flags](Expr.splice(prefix).runtimeData)
          }
        )(d =>
          Expr.quote {
            new io.scalaland.chimney.dsl.TransformerDefinition[From, To, O2, Flags](
              Expr.splice(d) +: Expr.splice(prefix).runtimeData
            )
          }
        )
        .as_??
    }

    def withFieldRenamed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selectorFrom: Expr[Any],
        selectorTo: Expr[Any]
    ): Expr_?? =
      withPathTypes(selectorFrom, selectorTo)((fromPath, toPath) =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          None,
          DslOverride.renamed(fromPath, toPath)(using TO.Empty)
        )
      )

    def withFieldConst[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        value: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(value),
          DslOverride.const(toPath)(using TO.Empty)
        )
      )

    def withFieldComputed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(f),
          DslOverride.computed(rootPathType, toPath)(using TO.Empty)
        )
      )

    def withFieldComputedPartial[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.TransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(f),
          DslOverride.computedPartial(rootPathType, toPath)(using TO.Empty)
        )
      )
  }

  object PartialTransformerDefinitionForAllDsl {

    private def make[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.PartialTransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        data: Option[Expr[Any]],
        inner: ??<:[runtime.TransformerOverrides]
    ): Expr_?? = {
      val forAllType: ??<:[runtime.TransformerOverrides] = DslOverride.forAll[FromMatch, ToMatch, Overrides](inner)
      import forAllType.Underlying as O2
      implicit val ResultType: Type[dsls.PartialTransformerDefinition[From, To, O2, Flags]] =
        PartialTransformerDefinitionT[From, To, O2, Flags]
      data
        .fold(
          Expr.quote {
            new io.scalaland.chimney.dsl.PartialTransformerDefinition[From, To, O2, Flags](
              Expr.splice(prefix).runtimeData
            )
          }
        )(d =>
          Expr.quote {
            new io.scalaland.chimney.dsl.PartialTransformerDefinition[From, To, O2, Flags](
              Expr.splice(d) +: Expr.splice(prefix).runtimeData
            )
          }
        )
        .as_??
    }

    def withFieldRenamed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.PartialTransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selectorFrom: Expr[Any],
        selectorTo: Expr[Any]
    ): Expr_?? =
      withPathTypes(selectorFrom, selectorTo)((fromPath, toPath) =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          None,
          DslOverride.renamed(fromPath, toPath)(using TO.Empty)
        )
      )

    def withFieldConst[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.PartialTransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        value: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(value),
          DslOverride.const(toPath)(using TO.Empty)
        )
      )

    def withFieldComputed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.PartialTransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(f),
          DslOverride.computed(rootPathType, toPath)(using TO.Empty)
        )
      )

    def withFieldComputedPartial[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.PartialTransformerDefinitionForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(f),
          DslOverride.computedPartial(rootPathType, toPath)(using TO.Empty)
        )
      )
  }

  object TransformerIntoForAllDsl {

    private def make[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.TransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        data: Option[Expr[Any]],
        inner: ??<:[runtime.TransformerOverrides]
    ): Expr_?? = {
      val forAllType: ??<:[runtime.TransformerOverrides] = DslOverride.forAll[FromMatch, ToMatch, Overrides](inner)
      import forAllType.Underlying as O2
      implicit val ResultType: Type[dsls.TransformerInto[From, To, O2, Flags]] =
        TransformerIntoT[From, To, O2, Flags]
      data
        .fold(
          Expr.quote {
            val ti = Expr.splice(prefix)
            new io.scalaland.chimney.dsl.TransformerInto[From, To, O2, Flags](
              ti.source,
              ti.td.asInstanceOf[io.scalaland.chimney.dsl.TransformerDefinition[From, To, O2, Flags]]
            )
          }
        )(d =>
          Expr.quote {
            val ti = Expr.splice(prefix)
            val updatedTd = io.scalaland.chimney.internal.runtime.WithRuntimeDataStore
              .update(ti.td, Expr.splice(d))
              .asInstanceOf[io.scalaland.chimney.dsl.TransformerDefinition[From, To, O2, Flags]]
            new io.scalaland.chimney.dsl.TransformerInto[From, To, O2, Flags](ti.source, updatedTd)
          }
        )
        .as_??
    }

    def withFieldRenamed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.TransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selectorFrom: Expr[Any],
        selectorTo: Expr[Any]
    ): Expr_?? =
      withPathTypes(selectorFrom, selectorTo)((fromPath, toPath) =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          None,
          DslOverride.renamed(fromPath, toPath)(using TO.Empty)
        )
      )

    def withFieldConst[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.TransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        value: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(value),
          DslOverride.const(toPath)(using TO.Empty)
        )
      )

    def withFieldComputed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.TransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(f),
          DslOverride.computed(rootPathType, toPath)(using TO.Empty)
        )
      )
  }

  object PartialTransformerIntoForAllDsl {

    private def make[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.PartialTransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        data: Option[Expr[Any]],
        inner: ??<:[runtime.TransformerOverrides]
    ): Expr_?? = {
      val forAllType: ??<:[runtime.TransformerOverrides] = DslOverride.forAll[FromMatch, ToMatch, Overrides](inner)
      import forAllType.Underlying as O2
      implicit val ResultType: Type[dsls.PartialTransformerInto[From, To, O2, Flags]] =
        PartialTransformerIntoT[From, To, O2, Flags]
      data
        .fold(
          Expr.quote {
            val ti = Expr.splice(prefix)
            new io.scalaland.chimney.dsl.PartialTransformerInto[From, To, O2, Flags](
              ti.source,
              ti.td.asInstanceOf[io.scalaland.chimney.dsl.PartialTransformerDefinition[From, To, O2, Flags]]
            )
          }
        )(d =>
          Expr.quote {
            val ti = Expr.splice(prefix)
            val updatedTd = io.scalaland.chimney.internal.runtime.WithRuntimeDataStore
              .update(ti.td, Expr.splice(d))
              .asInstanceOf[io.scalaland.chimney.dsl.PartialTransformerDefinition[From, To, O2, Flags]]
            new io.scalaland.chimney.dsl.PartialTransformerInto[From, To, O2, Flags](ti.source, updatedTd)
          }
        )
        .as_??
    }

    def withFieldRenamed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.PartialTransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selectorFrom: Expr[Any],
        selectorTo: Expr[Any]
    ): Expr_?? =
      withPathTypes(selectorFrom, selectorTo)((fromPath, toPath) =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          None,
          DslOverride.renamed(fromPath, toPath)(using TO.Empty)
        )
      )

    def withFieldConst[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.PartialTransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        value: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(value),
          DslOverride.const(toPath)(using TO.Empty)
        )
      )

    def withFieldComputed[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.PartialTransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(f),
          DslOverride.computed(rootPathType, toPath)(using TO.Empty)
        )
      )

    def withFieldComputedPartial[
        From: Type,
        To: Type,
        Overrides <: runtime.TransformerOverrides: Type,
        Flags <: runtime.TransformerFlags: Type,
        FromMatch: Type,
        ToMatch: Type
    ](
        prefix: Expr[dsls.PartialTransformerIntoForAll[From, To, Overrides, Flags, FromMatch, ToMatch]],
        selector: Expr[Any],
        f: Expr[Any]
    ): Expr_?? =
      withPathType(selector)(toPath =>
        make[From, To, Overrides, Flags, FromMatch, ToMatch](
          prefix,
          Some(f),
          DslOverride.computedPartial(rootPathType, toPath)(using TO.Empty)
        )
      )
  }

  // --- with{Source,Target,PatchedValue}Flag bodies (result classes differ per wrapper) ---

  def transformerDefinitionWithSourceFlag[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type
  ](
      prefix: Expr[dsls.TransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[Any]
  ): Expr_?? =
    withPathType(selectorFrom) { fromPath =>
      import fromPath.Underlying as FromPath
      implicit val ResultType
          : Type[dsls.TransformerSourceFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, FromPath]] =
        SourceFlagsOfTransformerDefinitionT[From, To, Overrides, Flags, FromPath]
      Expr.quote {
        new io.scalaland.chimney.dsl.TransformerSourceFlagsDsl.OfTransformerDefinition[
          From,
          To,
          Overrides,
          Flags,
          FromPath
        ](Expr.splice(prefix))
      }.as_??
    }

  def transformerDefinitionWithTargetFlag[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type
  ](
      prefix: Expr[dsls.TransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[Any]
  ): Expr_?? =
    withPathType(selectorTo) { toPath =>
      import toPath.Underlying as ToPath
      implicit val ResultType
          : Type[dsls.TransformerTargetFlagsDsl.OfTransformerDefinition[From, To, Overrides, Flags, ToPath]] =
        TargetFlagsOfTransformerDefinitionT[From, To, Overrides, Flags, ToPath]
      Expr.quote {
        new io.scalaland.chimney.dsl.TransformerTargetFlagsDsl.OfTransformerDefinition[
          From,
          To,
          Overrides,
          Flags,
          ToPath
        ](Expr.splice(prefix))
      }.as_??
    }

  def transformerIntoWithSourceFlag[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type
  ](
      prefix: Expr[dsls.TransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[Any]
  ): Expr_?? =
    withPathType(selectorFrom) { fromPath =>
      import fromPath.Underlying as FromPath
      implicit val ResultType
          : Type[dsls.TransformerSourceFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, FromPath]] =
        SourceFlagsOfTransformerIntoT[From, To, Overrides, Flags, FromPath]
      Expr.quote {
        new io.scalaland.chimney.dsl.TransformerSourceFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, FromPath](
          Expr.splice(prefix)
        )
      }.as_??
    }

  def transformerIntoWithTargetFlag[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type
  ](
      prefix: Expr[dsls.TransformerInto[From, To, Overrides, Flags]],
      selectorTo: Expr[Any]
  ): Expr_?? =
    withPathType(selectorTo) { toPath =>
      import toPath.Underlying as ToPath
      implicit val ResultType
          : Type[dsls.TransformerTargetFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, ToPath]] =
        TargetFlagsOfTransformerIntoT[From, To, Overrides, Flags, ToPath]
      Expr.quote {
        new io.scalaland.chimney.dsl.TransformerTargetFlagsDsl.OfTransformerInto[From, To, Overrides, Flags, ToPath](
          Expr.splice(prefix)
        )
      }.as_??
    }

  def partialTransformerDefinitionWithSourceFlag[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type
  ](
      prefix: Expr[dsls.PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorFrom: Expr[Any]
  ): Expr_?? =
    withPathType(selectorFrom) { fromPath =>
      import fromPath.Underlying as FromPath
      implicit val ResultType
          : Type[dsls.TransformerSourceFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, FromPath]] =
        SourceFlagsOfPartialTransformerDefinitionT[From, To, Overrides, Flags, FromPath]
      Expr.quote {
        new io.scalaland.chimney.dsl.TransformerSourceFlagsDsl.OfPartialTransformerDefinition[
          From,
          To,
          Overrides,
          Flags,
          FromPath
        ](Expr.splice(prefix))
      }.as_??
    }

  def partialTransformerDefinitionWithTargetFlag[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type
  ](
      prefix: Expr[dsls.PartialTransformerDefinition[From, To, Overrides, Flags]],
      selectorTo: Expr[Any]
  ): Expr_?? =
    withPathType(selectorTo) { toPath =>
      import toPath.Underlying as ToPath
      implicit val ResultType
          : Type[dsls.TransformerTargetFlagsDsl.OfPartialTransformerDefinition[From, To, Overrides, Flags, ToPath]] =
        TargetFlagsOfPartialTransformerDefinitionT[From, To, Overrides, Flags, ToPath]
      Expr.quote {
        new io.scalaland.chimney.dsl.TransformerTargetFlagsDsl.OfPartialTransformerDefinition[
          From,
          To,
          Overrides,
          Flags,
          ToPath
        ](Expr.splice(prefix))
      }.as_??
    }

  def partialTransformerIntoWithSourceFlag[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type
  ](
      prefix: Expr[dsls.PartialTransformerInto[From, To, Overrides, Flags]],
      selectorFrom: Expr[Any]
  ): Expr_?? =
    withPathType(selectorFrom) { fromPath =>
      import fromPath.Underlying as FromPath
      implicit val ResultType
          : Type[dsls.TransformerSourceFlagsDsl.OfPartialTransformerInto[From, To, Overrides, Flags, FromPath]] =
        SourceFlagsOfPartialTransformerIntoT[From, To, Overrides, Flags, FromPath]
      Expr.quote {
        new io.scalaland.chimney.dsl.TransformerSourceFlagsDsl.OfPartialTransformerInto[
          From,
          To,
          Overrides,
          Flags,
          FromPath
        ](Expr.splice(prefix))
      }.as_??
    }

  def partialTransformerIntoWithTargetFlag[
      From: Type,
      To: Type,
      Overrides <: runtime.TransformerOverrides: Type,
      Flags <: runtime.TransformerFlags: Type
  ](
      prefix: Expr[dsls.PartialTransformerInto[From, To, Overrides, Flags]],
      selectorTo: Expr[Any]
  ): Expr_?? =
    withPathType(selectorTo) { toPath =>
      import toPath.Underlying as ToPath
      implicit val ResultType
          : Type[dsls.TransformerTargetFlagsDsl.OfPartialTransformerInto[From, To, Overrides, Flags, ToPath]] =
        TargetFlagsOfPartialTransformerIntoT[From, To, Overrides, Flags, ToPath]
      Expr.quote {
        new io.scalaland.chimney.dsl.TransformerTargetFlagsDsl.OfPartialTransformerInto[
          From,
          To,
          Overrides,
          Flags,
          ToPath
        ](Expr.splice(prefix))
      }.as_??
    }

  def patcherDefinitionWithPatchedValueFlag[
      A: Type,
      Patch: Type,
      Overrides <: runtime.PatcherOverrides: Type,
      Flags <: runtime.PatcherFlags: Type
  ](
      prefix: Expr[dsls.PatcherDefinition[A, Patch, Overrides, Flags]],
      selectorObj: Expr[Any]
  ): Expr_?? =
    withPathType(selectorObj) { objPath =>
      import objPath.Underlying as ObjPath
      implicit val ResultType
          : Type[dsls.PatcherPatchedValueFlagsDsl.OfPatcherDefinition[A, Patch, Overrides, Flags, ObjPath]] =
        PatchedValueFlagsOfPatcherDefinitionT[A, Patch, Overrides, Flags, ObjPath]
      Expr.quote {
        new io.scalaland.chimney.dsl.PatcherPatchedValueFlagsDsl.OfPatcherDefinition[
          A,
          Patch,
          Overrides,
          Flags,
          ObjPath
        ](
          Expr.splice(prefix)
        )
      }.as_??
    }

  def patcherUsingWithPatchedValueFlag[
      A: Type,
      Patch: Type,
      Overrides <: runtime.PatcherOverrides: Type,
      Flags <: runtime.PatcherFlags: Type
  ](
      prefix: Expr[dsls.PatcherUsing[A, Patch, Overrides, Flags]],
      selectorObj: Expr[Any]
  ): Expr_?? =
    withPathType(selectorObj) { objPath =>
      import objPath.Underlying as ObjPath
      implicit val ResultType
          : Type[dsls.PatcherPatchedValueFlagsDsl.OfPatcherUsing[A, Patch, Overrides, Flags, ObjPath]] =
        PatchedValueFlagsOfPatcherUsingT[A, Patch, Overrides, Flags, ObjPath]
      Expr.quote {
        new io.scalaland.chimney.dsl.PatcherPatchedValueFlagsDsl.OfPatcherUsing[A, Patch, Overrides, Flags, ObjPath](
          Expr.splice(prefix)
        )
      }.as_??
    }
}
