package io.scalaland.chimney.internal.compiletime.dsl

import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.internal.compiletime.dsl.utils.DslMacroUtils
import io.scalaland.chimney.internal.runtime.{Path, TransformerFlags, TransformerOverrides}
import io.scalaland.chimney.internal.runtime.TransformerOverrides.*

import scala.quoted.*

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
  )(using Quotes): Expr[CodecDefinition[Domain, Dto, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]] =
    DslMacroUtils().applyFieldNameTypes {
      [fromPath <: Path, toPath <: Path] =>
        (_: Type[fromPath]) ?=>
          (_: Type[toPath]) ?=>
            '{
              $cd.asInstanceOf[CodecDefinition[
                Domain,
                Dto,
                Renamed[fromPath, toPath, EncodeOverrides],
                Renamed[toPath, fromPath, DecodeOverrides],
                Flags
              ]]
          }
    }(selectorDomain, selectorDto)

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
  )(using Quotes): Expr[CodecDefinition[Domain, Dto, ? <: TransformerOverrides, ? <: TransformerOverrides, Flags]] =
    '{
      $cd
        .asInstanceOf[CodecDefinition[
          Domain,
          Dto,
          Renamed[
            Path.SourceMatching[Path.Root, DomainSubtype],
            Path.Matching[Path.Root, DtoSubtype],
            EncodeOverrides
          ],
          Renamed[
            Path.SourceMatching[Path.Root, DtoSubtype],
            Path.Matching[Path.Root, DomainSubtype],
            DecodeOverrides
          ],
          Flags
        ]]
    }
}
