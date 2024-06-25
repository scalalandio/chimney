package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.dsl.utils.DslMacroUtils
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.quoted.*

object IsoDefinitionMacros {

  def withFieldRenamedImpl[
      From: Type,
      To: Type,
      FromOverrides <: TransformerOverrides: Type,
      ToOverrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      id: Expr[IsoDefinition[From, To, FromOverrides, ToOverrides, Flags]],
      selectorFrom: Expr[From => T],
      selectorTo: Expr[To => U]
  )(using Quotes): Expr[IsoDefinition[From, To, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              $id.asInstanceOf[IsoDefinition[
                From,
                To,
                RenamedFrom[fromPath, toPath, FromOverrides],
                RenamedFrom[toPath, fromPath, ToOverrides],
                Flags
              ]]
          }
    }(selectorFrom, selectorTo)
}
