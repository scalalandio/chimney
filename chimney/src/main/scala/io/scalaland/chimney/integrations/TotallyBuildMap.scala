package io.scalaland.chimney.integrations

/** Tells Chimney how to interact with `Collection` type to align its behavior with e.g. [[Map]].
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#custom-collection-types]] for more details
  *
  * @tparam Mapp
  *   type storing collection of `Key`-`Value` pairs - has to be proper type, not higher-kinded type
  * @tparam Key
  *   type of internal keys
  * @tparam Value
  *   type of internal values
  *
  * @since 1.0.0
  */
trait TotallyBuildMap[Mapp, Key, Value] extends TotallyBuildIterable[Mapp, (Key, Value)]
