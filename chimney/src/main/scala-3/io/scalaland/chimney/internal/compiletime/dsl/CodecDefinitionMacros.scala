package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.PlatformBridge
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}

import scala.quoted.*

final class CodecDefinitionMacros(q: Quotes) extends PlatformBridge(q) with DslMacros

object CodecDefinitionMacros {

  def withFieldRenamedImpl[
      Domain: Type,
      Dto: Type,
      EncodeOverrides <: TransformerOverrides: Type,
      DecodeOverrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      T: Type,
      U: Type
  ](
      cd: Expr[CodecDefinition[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags]],
      selectorDomain: Expr[Domain => T],
      selectorDto: Expr[Dto => U]
  )(using Quotes): Expr[CodecDefinition[Domain, Dto, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]] = {
    val m = new CodecDefinitionMacros(quotes)
    m.CodecDefinitionDsl
      .withFieldRenamed[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags](cd, selectorDomain, selectorDto)
      .value
      .asInstanceOf[Expr[CodecDefinition[Domain, Dto, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]]]
  }

  def withSealedSubtypeRenamedImpl[
      Domain: Type,
      Dto: Type,
      EncodeOverrides <: TransformerOverrides: Type,
      DecodeOverrides <: TransformerOverrides: Type,
      Flags <: TransformerFlags: Type,
      DomainSubtype: Type,
      DtoSubtype: Type
  ](
      cd: Expr[CodecDefinition[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags]]
  )(using Quotes): Expr[CodecDefinition[Domain, Dto, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]] = {
    val m = new CodecDefinitionMacros(quotes)
    m.CodecDefinitionDsl
      .withSealedSubtypeRenamed[Domain, Dto, EncodeOverrides, DecodeOverrides, Flags](
        cd,
        m.typeOf_??[DomainSubtype],
        m.typeOf_??[DtoSubtype]
      )
      .value
      .asInstanceOf[Expr[CodecDefinition[Domain, Dto, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]]]
  }
}
