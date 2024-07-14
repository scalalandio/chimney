package io.scalaland.chimney.internal.compiletime

trait Definitions extends Types with Existentials with Exprs with ExprPromises with Results {

  /** Values passed into scalacOptions with "-Xmacro-settings:...", can be used to define global settings. */
  protected def XMacroSettings: List[String]
}
