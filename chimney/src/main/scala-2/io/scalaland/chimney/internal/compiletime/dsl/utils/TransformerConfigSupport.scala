package io.scalaland.chimney.internal.compiletime.dsl.utils

import scala.reflect.macros.blackbox

trait TransformerConfigSupport extends MacroUtils {

  val c: blackbox.Context

  import c.universe.*

  object CfgTpes {

    import io.scalaland.chimney.internal.TransformerCfg.*

    // We cannot get typeOf[HigherKind] directly, but we can get the typeOf[ExistentialType]
    // and extract type constructor out of it.

    val emptyT: Type = typeOf[Empty]
    val fieldConstT: Type = typeOf[FieldConst[?, ?]].typeConstructor
    val fieldConstPartialT: Type = typeOf[FieldConstPartial[?, ?]].typeConstructor
    val fieldComputedT: Type = typeOf[FieldComputed[?, ?]].typeConstructor
    val fieldComputedPartialT: Type = typeOf[FieldComputedPartial[?, ?]].typeConstructor
    val fieldRelabelledT: Type = typeOf[FieldRelabelled[?, ?, ?]].typeConstructor
    val coproductInstanceT: Type = typeOf[CoproductInstance[?, ?, ?]].typeConstructor
    val coproductInstancePartialT: Type = typeOf[CoproductInstancePartial[?, ?, ?]].typeConstructor
  }
}
