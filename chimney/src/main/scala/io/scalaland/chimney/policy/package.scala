package io.scalaland.chimney

package object policy {

  /** Import this value to permit Chimney's structural derivation in the current scope when the derivation policy is
    * `opt-in`:
    *
    * {{{
    * import io.scalaland.chimney.policy.allowDerivationForChimney
    * }}}
    *
    * Configured with (see the derivation-policy chapter of the documentation):
    *
    * {{{
    * -Xmacro-settings:chimney.policy.enabled=opt-in
    * -Xmacro-settings:chimney.policy.allowedScopes=com.acme.mappings;com.acme.Instances
    * -Xmacro-settings:chimney.policy.optInByImport=true
    * }}}
    *
    * A no-op under the default `always-allowed` policy.
    *
    * @since 2.0.0
    */
  implicit val allowDerivationForChimney: AllowDerivation = new AllowDerivation
}
