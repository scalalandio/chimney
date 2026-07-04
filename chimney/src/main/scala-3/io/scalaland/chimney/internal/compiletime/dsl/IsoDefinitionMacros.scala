package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}

import scala.quoted.*

final class IsoDefinitionMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object IsoDefinitionMacros {

  def withFieldRenamedImpl[
      First: Type,
      Second: Type,
      FirstOverrides <: TransformerOverrides: Type,
      SecondOverrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      id: Expr[IsoDefinition[First, Second, FirstOverrides, SecondOverrides, Flags]],
      selectorFirst: Expr[First => T],
      selectorSecond: Expr[Second => U]
  )(using Quotes): Expr[IsoDefinition[First, Second, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]] = {
    val m = new IsoDefinitionMacros(quotes)
    m.IsoDefinitionDsl
      .withFieldRenamed[First, Second, FirstOverrides, SecondOverrides, Flags](id, selectorFirst, selectorSecond)
      .value
      .asInstanceOf[Expr[IsoDefinition[First, Second, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeRenamedImpl[
      First: Type,
      Second: Type,
      FirstOverrides <: TransformerOverrides: Type,
      SecondOverrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      FirstSubtype: Type,
      SecondSubtype: Type
  ](
      id: Expr[IsoDefinition[First, Second, FirstOverrides, SecondOverrides, Flags]]
  )(using Quotes): Expr[IsoDefinition[First, Second, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]] = {
    val m = new IsoDefinitionMacros(quotes)
    m.IsoDefinitionDsl
      .withSealedSubtypeRenamed[First, Second, FirstOverrides, SecondOverrides, Flags](
        id,
        m.typeOf_??[FirstSubtype],
        m.typeOf_??[SecondSubtype]
      )
      .value
      .asInstanceOf[Expr[IsoDefinition[First, Second, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]]]
  }
}
