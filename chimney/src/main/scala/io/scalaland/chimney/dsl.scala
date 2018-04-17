package io.scalaland.chimney

import io.scalaland.chimney.internal.{ChimneyMacros, DisableDefaults, Lst, Cns, Nl}

import scala.language.experimental.macros

object dsl {

  implicit class TransformerOps[From](val source: From) extends AnyVal {

    final def into[To]: TransformerInto[From, To, Nl] =
      new TransformerInto[From, To, Nl](source, Map.empty)

    final def transformInto[To](implicit transformer: Transformer[From, To]): To =
      transformer.transform(source)
  }

  final class TransformerInto[From, To, Overrides <: Lst](val source: From, val overrides: Map[String, Any]) {

    def disableDefaultValues: TransformerInto[From, To, Cns[DisableDefaults, Overrides]] =
      new TransformerInto[From, To, Cns[DisableDefaults, Overrides]](source, overrides)

    def withFieldConst[T](selector: To => T, value: T): TransformerInto[From, To, _] =
      macro ChimneyMacros.withFieldConstImpl2[From, To, T, Overrides]

    def withFieldComputed[T](selector: To => T, map: From => T): TransformerInto[From, To, _] =
      macro ChimneyMacros.withFieldComputedImpl2[From, To, T, Overrides]

    def transform: To =
      macro ChimneyMacros.transformImpl[From, To, Overrides]

//    @inline def addConstOverride[Overrides2 <: Lst](fieldName: String,
//                                                    const: Any): TransformerInto[From, To, Overrides2] =
//      new TransformerInto[From, To, Overrides2](source, overrides.updated(fieldName, const))
//
//    @inline def addComputedOverride[Overrides2 <: Lst](fieldName: String,
//                                                       function: From => Any): TransformerInto[From, To, Overrides2] =
//      new TransformerInto[From, To, Overrides2](source, overrides.updated(fieldName, function(source)))
  }
}
