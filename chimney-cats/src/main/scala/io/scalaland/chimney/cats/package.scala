package io.scalaland.chimney

/** Since 2.0.0 this package provides ONLY:
  *   - Cats TYPECLASS instances for Chimney's types (`Transformer`, `PartialTransformer`, `partial.Result`, `Iso`,
  *     `Codec`) - consumed by Cats' own syntax/derivation, and
  *   - extension methods/`AsResult` instances bridging `partial.Result` with `cats.data.Validated`.
  *
  * The `io.scalaland.chimney.integrations.*` instances for Cats collections (`Chain`, `NonEmptyList`, `NonEmptyMap`,
  * ...) that used to live here (`CatsDataImplicits`) were REMOVED: Chimney's derivation now consults Hearth
  * `StandardMacroExtension` providers, so putting `com.kubuszok::kindlings-cats-integration` on the compile classpath
  * enables these conversions WITHOUT any import (smart-constructor types like `NonEmptyList` derive with
  * `PartialTransformer`; `Chain` derives totally).
  *
  * @since 0.5.0
  */
package object cats
    extends CatsTotalTransformerImplicits
    with CatsPartialTransformerImplicits
    with CatsPartialResultImplicits
    with CatsCodecImplicits
    with CatsIsoImplicits
