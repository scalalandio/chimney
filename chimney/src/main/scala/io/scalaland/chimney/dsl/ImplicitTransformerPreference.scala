package io.scalaland.chimney.dsl

/** Whether derivation should prefer total or partial transformers if both are provided for some field transformation.
  *
  * @see [[https://chimney.readthedocs.io/supported-transformations/#resolving-priority-of-implicit-total-vs-partial-transformers]] for more details
  *
  * @since 0.7.0
  */
sealed abstract class ImplicitTransformerPreference

/** Tell the derivation to prefer total transformers.
  *
  * @see [[https://chimney.readthedocs.io/supported-transformations/#resolving-priority-of-implicit-total-vs-partial-transformers]] for more details
  *
  * @since 0.7.0
  */
case object PreferTotalTransformer extends ImplicitTransformerPreference

/** Tell the derivation to prefer partial transformers.
  *
  * @see [[https://chimney.readthedocs.io/supported-transformations/#resolving-priority-of-implicit-total-vs-partial-transformers]] for more details
  *
  * @since 0.7.0
  */
case object PreferPartialTransformer extends ImplicitTransformerPreference
