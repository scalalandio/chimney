package io.scalaland.chimney.internal.compiletime.derivation.patcher

import io.scalaland.chimney.{dsl, Patcher}
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.compiletime.derivation.RuntimeDataStoreFlattening
import io.scalaland.chimney.internal.runtime

import scala.reflect.macros.blackbox

final class PatcherMacros(ctx: blackbox.Context) extends PlatformBridge(ctx) with Derivation with Gateway {

  import c.universe.{internal as _, Transformer as _, *}

  def derivePatchWithConfig[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: runtime.PatcherOverrides: WeakTypeTag,
      Flags <: runtime.PatcherFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.PatcherFlags: WeakTypeTag
  ](
      pc: Expr[io.scalaland.chimney.dsl.PatcherConfiguration[ImplicitScopeFlags]]
  ): c.Expr[A] = retypecheck {
    import RuntimeDataStoreFlattening.{EmptyBase, OpaqueBase}
    // Called by PatcherUsing => prefix is PatcherUsing (constructor args: obj, objPatch, pd)
    val (base, data) = RuntimeDataStoreFlattening.analyzeChain(c.prefix.tree)(c)
    val result: Option[c.Expr[A]] = base match {
      case EmptyBase =>
        RuntimeDataStoreFlattening.extractBaseConstructorArgs(c.prefix.tree)(c).collect {
          case args if args.sizeIs >= 2 =>
            val rds =
              if (data.nonEmpty)
                c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](
                  q"_root_.io.scalaland.chimney.internal.runtime.RuntimeDataStore.wrap(_root_.scala.Array[Any](..$data))"
                )
              else
                c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](
                  q"_root_.io.scalaland.chimney.internal.runtime.RuntimeDataStore.empty"
                )
            val body = derivePatcherResult[A, Patch, Overrides, Flags, ImplicitScopeFlags](
              obj = c.Expr[A](args(0)),
              patch = c.Expr[Patch](args(1)),
              runtimeDataStore = rds
            )
            c.Expr[A](q"{ ${Expr.suppressUnused(pc)}; $body }")
        }
      case OpaqueBase(baseTree) =>
        val bt = baseTree.asInstanceOf[c.universe.Tree]
        val baseName = TermName(c.freshName("chainBase"))
        val rds =
          if (data.nonEmpty)
            c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](
              q"$baseName.pd.runtimeData.prependedAll(_root_.scala.Array[Any](..$data))"
            )
          else
            c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](q"$baseName.pd.runtimeData")
        val body = derivePatcherResult[A, Patch, Overrides, Flags, ImplicitScopeFlags](
          obj = c.Expr[A](q"$baseName.obj"),
          patch = c.Expr[Patch](q"$baseName.objPatch"),
          runtimeDataStore = rds
        )
        Some(c.Expr[A](q"{ val $baseName = $bt; ${Expr.suppressUnused(pc)}; $body }"))
    }
    result.getOrElse(
      cacheDefinition(c.Expr[dsl.PatcherUsing[A, Patch, Overrides, Flags]](c.prefix.tree)) { pu =>
        val body = derivePatcherResult[A, Patch, Overrides, Flags, ImplicitScopeFlags](
          obj = c.Expr[A](q"$pu.obj"),
          patch = c.Expr[Patch](q"$pu.objPatch"),
          runtimeDataStore = c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](q"$pu.pd.runtimeData")
        )
        c.Expr[A](q"{ ${Expr.suppressUnused(pc)}; $body }")
      }
    )
  }

  def derivePatcherWithConfig[
      A: WeakTypeTag,
      Patch: WeakTypeTag,
      Overrides <: runtime.PatcherOverrides: WeakTypeTag,
      InstanceFlags <: runtime.PatcherFlags: WeakTypeTag,
      ImplicitScopeFlags <: runtime.PatcherFlags: WeakTypeTag
  ](
      pc: Expr[io.scalaland.chimney.dsl.PatcherConfiguration[ImplicitScopeFlags]]
  ): Expr[Patcher[A, Patch]] = retypecheck {
    import RuntimeDataStoreFlattening.{EmptyBase, OpaqueBase}
    // Called by PatcherDefinition => prefix is PatcherDefinition
    val (base, data) = RuntimeDataStoreFlattening.analyzeChain(c.prefix.tree)(c)
    val rds: c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore] = base match {
      case EmptyBase if data.nonEmpty =>
        c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](
          q"_root_.io.scalaland.chimney.internal.runtime.RuntimeDataStore.wrap(_root_.scala.Array[Any](..$data))"
        )
      case EmptyBase =>
        c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](
          q"_root_.io.scalaland.chimney.internal.runtime.RuntimeDataStore.empty"
        )
      case OpaqueBase(baseTree) if data.nonEmpty =>
        val bt = baseTree.asInstanceOf[c.universe.Tree]
        c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](
          q"$bt.runtimeData.prependedAll(_root_.scala.Array[Any](..$data))"
        )
      case _ =>
        c.Expr[dsl.PatcherDefinitionCommons.RuntimeDataStore](q"${c.prefix.tree}.runtimeData")
    }
    val body = derivePatcher[A, Patch, Overrides, InstanceFlags, ImplicitScopeFlags](rds)
    c.Expr[Patcher[A, Patch]](q"{ ${Expr.suppressUnused(pc)}; $body }")
  }

  def derivePatcherWithDefaults[
      A: WeakTypeTag,
      Patch: WeakTypeTag
  ]: Expr[Patcher[A, Patch]] = retypecheck(
    resolveImplicitScopeConfigAndMuteUnusedWarnings { implicitScopeFlagsType =>
      import implicitScopeFlagsType.Underlying as ImplicitScopeFlags
      derivePatcher[A, Patch, runtime.PatcherOverrides.Empty, runtime.PatcherFlags.Default, ImplicitScopeFlags](
        ChimneyExpr.RuntimeDataStore.empty
      )
    }
  )

  private def resolveImplicitScopeConfigAndMuteUnusedWarnings[A: Type](
      useImplicitScopeFlags: ??<:[runtime.PatcherFlags] => Expr[A]
  ): Expr[A] = {
    val implicitScopeConfig = {
      val patcherConfigurationType =
        c.WeakTypeTag[io.scalaland.chimney.dsl.PatcherConfiguration[? <: runtime.PatcherFlags]](
          c.typecheck(
            tree = tq"${typeOf[io.scalaland.chimney.dsl.PatcherConfiguration[? <: runtime.PatcherFlags]]}",
            silent = true,
            mode = c.TYPEmode,
            withImplicitViewsDisabled = true,
            withMacrosDisabled = false
          ).tpe
        )

      Expr.summonImplicit(patcherConfigurationType).toOption.getOrElse {
        // $COVERAGE-OFF$should never happen unless someone mess around with type-level representation
        reportError("Can't locate implicit PatcherConfiguration!")
        // $COVERAGE-ON$
      }
    }
    val implicitScopeFlagsType = c
      .WeakTypeTag[runtime.PatcherFlags](implicitScopeConfig.tpe.tpe.typeArgs.head)
      .as_??<:[runtime.PatcherFlags]

    val body = useImplicitScopeFlags(implicitScopeFlagsType)
    c.Expr[A](q"{ ${Expr.suppressUnused(implicitScopeConfig)}; $body }")
  }

  private def retypecheck[A: Type](expr: c.Expr[A]): c.Expr[A] = try
    c.Expr[A](c.typecheck(tree = c.untypecheck(expr.tree)))
  catch {
    case scala.reflect.macros.TypecheckException(_, msg) => c.abort(c.enclosingPosition, msg)
  }
}
