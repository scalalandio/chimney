package io.scalaland.chimney.internal.compiletime.derivation

import hearth.fp.effect.MIO
import io.scalaland.chimney.internal.compiletime.DerivationError

/** Settings-driven gate controlling '''where''' Chimney's structural derivation (generating transformation code for
  * `case class`es and `sealed` hierarchies / `enum`s) is allowed to happen. Direct port of Kindlings'
  * `DerivationPolicy` (kubuszok/kindlings#85 - the "sanely-automatic" derivation-policy design), with the single
  * `chimney` namespace instead of Kindlings' per-module ones.
  *
  * By default (`always-allowed`) nothing changes. Under `opt-in`, structural derivation is permitted only in designated
  * scopes or behind the `io.scalaland.chimney.policy.allowDerivationForChimney` import; everywhere else the macro fails
  * with an actionable message. Everything non-structural keeps working unconditionally: pre-existing implicits, subtype
  * upcasts, options, eithers, collections, value classes - the policy rule sits directly before the
  * ProductToProduct/SealedHierarchy (and PatchProductWithProduct) rules, so only the structural fallthrough is gated.
  *
  * Configuration keys (all under `chimney.policy.`):
  *
  * {{{
  * -Xmacro-settings:chimney.policy.enabled=opt-in            // or always-allowed (default => current behavior)
  * -Xmacro-settings:chimney.policy.allowedScopes=com.acme.mappings;com.acme.Instances
  * -Xmacro-settings:chimney.policy.optInByImport=true        // default true
  * }}}
  *
  * '''Why `;`/`|` and not `,` for `allowedScopes`''': the Scala 3 compiler splits a single `-Xmacro-settings:a,b`
  * option on commas into `List("a","b")`, while Scala 2 keeps it as one `"a,b"` string - so a comma is not portable.
  * `;` and `|` are never split by either compiler, so the value arrives intact and we split it ourselves.
  */
private[compiletime] trait DerivationPolicy { this: hearth.MacroCommons =>
  // $COVERAGE-OFF$settings-driven behavior is exercised via unit tests of the pure core + documentation snippets

  /** Computed once per macro expansion. On the default `always-allowed` path this short-circuits before touching
    * [[Environment.enclosingScope]] or summoning the marker implicit, keeping the common path cheap.
    */
  protected lazy val derivationPolicyDecision: DerivationPolicy.Decision = {
    val policyData = for {
      data <- Environment.typedSettings.toOption
      chimney <- data.get("chimney")
      policy <- chimney.get("policy")
    } yield policy

    val mode = policyData.flatMap(_.get("enabled")).flatMap(_.asString) match {
      case None      => DerivationPolicy.Mode.AlwaysAllowed
      case Some(raw) =>
        DerivationPolicy.parseMode(raw).getOrElse {
          Environment.reportWarn(
            s"chimney.policy.enabled: unrecognized value '$raw'. " +
              s"Expected 'always-allowed' or 'opt-in'. Using 'always-allowed'."
          )
          DerivationPolicy.Mode.AlwaysAllowed
        }
    }

    mode match {
      case DerivationPolicy.Mode.AlwaysAllowed => DerivationPolicy.Decision.Allowed
      case DerivationPolicy.Mode.OptIn         =>
        val allowedScopes =
          policyData
            .flatMap(_.get("allowedScopes"))
            .flatMap(_.asString)
            .map(DerivationPolicy.splitScopes)
            .getOrElse(Nil)
        val optInByImport =
          policyData.flatMap(_.get("optInByImport")).flatMap(_.asBoolean).getOrElse(true)
        val enclosureFullNames = enclosingScope.toList.flatMap(_.fullName)
        DerivationPolicy.decide(
          allowedScopes = allowedScopes,
          enclosureFullNames = enclosureFullNames,
          optInByImport = optInByImport,
          markerInScope = isDerivationOptInMarkerInScope
        )
    }
  }

  private def isDerivationOptInMarkerInScope: Boolean = {
    implicit val AllowDerivationType: Type[io.scalaland.chimney.AllowDerivation] =
      Type.of[io.scalaland.chimney.AllowDerivation]
    Expr.summonImplicit[io.scalaland.chimney.AllowDerivation].isDefined
  }

  private var derivationPolicyChecked = false

  /** The single policy check, run at most once per macro expansion. Yields `()` when derivation is permitted here
    * (recording, the first time, that the check passed so all nested derivations skip it); fails the [[MIO]] with a
    * [[DerivationError.PolicyViolation]] when denied.
    *
    * Intended to be the body of a rule placed directly BEFORE the structural rules: everything non-structural was
    * already handled by the earlier rules, and once the outermost structural derivation is permitted every nested one
    * is too.
    */
  protected def checkDerivationPolicyOncePerExpansion(derivationName: => String): MIO[Unit] =
    MIO.pure(()).flatMap { _ =>
      if (derivationPolicyChecked) MIO.pure(())
      else
        derivationPolicyDecision match {
          case DerivationPolicy.Decision.Allowed =>
            derivationPolicyChecked = true
            MIO.pure(())
          case denied: DerivationPolicy.Decision.Denied =>
            MIO.fail(DerivationError.PolicyViolation(derivationPolicyDeniedMessage(derivationName, denied)))
        }
    }

  private def derivationPolicyDeniedMessage(
      derivationName: String,
      denied: DerivationPolicy.Decision.Denied
  ): String = {
    val base =
      if (denied.allowedScopes.nonEmpty)
        s"""Structural derivation of $derivationName is enabled only in the following scopes:
           |${denied.allowedScopes.map(" - " + _).mkString("\n")}
           |
           |Currently you are in the following scope: ${denied.currentScope.getOrElse("<unknown>")}.""".stripMargin
      else
        s"Structural derivation of $derivationName is globally disabled."

    if (denied.optInByImport)
      base +
        s"""|
            |
            |You are allowed to enable this derivation locally by adding the import:
            |import io.scalaland.chimney.policy.allowDerivationForChimney""".stripMargin
    else base
  }
  // $COVERAGE-ON$
}

private[compiletime] object DerivationPolicy {

  /** Parsed value of `chimney.policy.enabled`. */
  sealed trait Mode extends Product with Serializable
  object Mode {
    case object AlwaysAllowed extends Mode
    case object OptIn extends Mode
  }

  /** Outcome of evaluating the policy for the current macro-expansion point. */
  sealed trait Decision extends Product with Serializable
  object Decision {
    case object Allowed extends Decision
    final case class Denied(currentScope: Option[String], allowedScopes: List[String], optInByImport: Boolean)
        extends Decision
  }

  def parseMode(raw: String): Option[Mode] = raw.trim.toLowerCase match {
    case "always-allowed" | "always-allow" | "always" | "allowed" => Some(Mode.AlwaysAllowed)
    case "opt-in" | "optin"                                       => Some(Mode.OptIn)
    case _                                                        => None
  }

  /** Splits an `allowedScopes` setting value on `;` or `|` (NOT `,` - see [[DerivationPolicy]] for why), trimming and
    * dropping empty entries.
    */
  def splitScopes(raw: String): List[String] =
    raw.split("[;|]").iterator.map(_.trim).filter(_.nonEmpty).toList

  /** Package-prefix-aware scope match: an `allowed` entry matches `fullName` when it is equal to it, or is a
    * dotted-segment / member prefix of it (so a package entry covers everything nested under it, and an object/class
    * entry covers its inner derivations). Boundaries recognized: `.`, `#`, `$`.
    */
  def scopeMatches(allowed: String, fullName: String): Boolean = {
    val a = allowed.trim
    a.nonEmpty && (fullName == a ||
      fullName.startsWith(a + ".") ||
      fullName.startsWith(a + "#") ||
      fullName.startsWith(a + "$"))
  }

  def scopeAllows(allowedScopes: List[String], enclosureFullNames: List[String]): Boolean =
    allowedScopes.exists(a => enclosureFullNames.exists(fn => scopeMatches(a, fn)))

  /** Pure core of the decision (unit-tested directly). `markerInScope` is by-name so the (non-free) marker-implicit
    * summon is only forced when the scope check has already failed and opt-in-by-import is enabled.
    */
  def decide(
      allowedScopes: List[String],
      enclosureFullNames: List[String],
      optInByImport: Boolean,
      markerInScope: => Boolean
  ): Decision =
    if (scopeAllows(allowedScopes, enclosureFullNames)) Decision.Allowed
    else if (optInByImport && markerInScope) Decision.Allowed
    else Decision.Denied(enclosureFullNames.headOption, allowedScopes, optInByImport)
}
