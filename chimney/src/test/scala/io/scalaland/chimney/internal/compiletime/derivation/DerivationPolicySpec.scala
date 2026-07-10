package io.scalaland.chimney.internal.compiletime.derivation

import io.scalaland.chimney.ChimneySpec

/** Unit tests of the pure decision core (port of Kindlings' DerivationPolicySpec) - the settings-driven wiring is
  * compile-time-global, so end-to-end behavior is exercised through documentation snippets instead.
  */
class DerivationPolicySpec extends ChimneySpec {

  import DerivationPolicy.*

  test("parseMode accepts documented spellings and rejects everything else") {
    parseMode("always-allowed") ==> Some(Mode.AlwaysAllowed)
    parseMode("Always") ==> Some(Mode.AlwaysAllowed)
    parseMode(" opt-in ") ==> Some(Mode.OptIn)
    parseMode("optin") ==> Some(Mode.OptIn)
    parseMode("nope") ==> None
  }

  test("splitScopes splits on ; and | but never on ,") {
    splitScopes("a.b;c.d") ==> List("a.b", "c.d")
    splitScopes("a.b | c.d") ==> List("a.b", "c.d")
    splitScopes("a,b") ==> List("a,b")
    splitScopes(" ; ;a.b; ") ==> List("a.b")
  }

  test("scopeMatches is a segment-aware prefix match") {
    scopeMatches("com.acme", "com.acme") ==> true
    scopeMatches("com.acme", "com.acme.json.Codecs") ==> true
    scopeMatches("com.acme.Codecs", "com.acme.Codecs$Inner") ==> true
    scopeMatches("com.acme.Codecs", "com.acme.Codecs#member") ==> true
    scopeMatches("com.acme", "com.acmeister") ==> false
    scopeMatches("", "com.acme") ==> false
  }

  test("decide: allowed scope wins without forcing the marker") {
    var markerForced = false
    decide(
      allowedScopes = List("com.acme"),
      enclosureFullNames = List("com.acme.json.Instances"),
      optInByImport = true,
      markerInScope = { markerForced = true; true }
    ) ==> Decision.Allowed
    markerForced ==> false
  }

  test("decide: marker allows when scopes do not match and optInByImport is on") {
    decide(List("com.acme"), List("com.other.Handler"), optInByImport = true, markerInScope = true) ==>
      Decision.Allowed
    decide(List("com.acme"), List("com.other.Handler"), optInByImport = false, markerInScope = true) ==>
      Decision.Denied(Some("com.other.Handler"), List("com.acme"), optInByImport = false)
  }

  test("decide: denial carries the current scope and configured scopes for the error message") {
    decide(Nil, List("com.other.Handler"), optInByImport = true, markerInScope = false) ==>
      Decision.Denied(Some("com.other.Handler"), Nil, optInByImport = true)
  }
}
