package io.scalaland.chimney

/** Marker used to permit Chimney's structural derivation in the current scope when the derivation policy is set to
  * `opt-in` (see the derivation-policy chapter of the documentation).
  *
  * Do not instantiate it directly - `import io.scalaland.chimney.policy.allowDerivationForChimney` in the scope where
  * the derivation should be allowed. Same idiom as Kindlings' per-module `policy` imports.
  *
  * A no-op under the default `always-allowed` policy.
  *
  * @see
  *   [[https://chimney.readthedocs.io Chimney documentation - derivation policy chapter]]
  *
  * @since 2.0.0
  */
final class AllowDerivation
