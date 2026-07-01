package io.scalaland.chimney.internal.compiletime2.datatypes

/** Loads Hearth's standard extensions ([[hearth.std.StdExtensions]] providers: `IsCollection`, `IsMap`, `IsOption`,
  * `IsEither`, `IsValueType`, `CtorLikes`) exactly once per macro-bundle instance.
  *
  * `IsCollection.unapply` & co. return nothing until `Environment.loadStandardExtensions()` has been called, but the
  * call must not be repeated within one expansion (redundant `ServiceLoader` scans + "already loaded" info logs -
  * see the hearth-standard-extensions pattern). The `var` lives on this trait, which is mixed exactly once into the
  * final macro-bundle class, making the guard per-expansion.
  *
  * The datatypes adapters themselves deliberately do NOT depend on standard extensions (see their ScalaDocs) - this
  * hook exists for the Gateway/rules layer (e.g. `IsOption`/`IsEither` based rewrites). It is effect-free on purpose
  * (the datatypes layer returns plain values; the rules phase decides the effectful API - an `MIO`-returning variant
  * can wrap this one later).
  */
private[compiletime2] trait StdExtensionsLoading { this: hearth.MacroCommons & hearth.std.StdExtensions =>

  private var standardExtensionsLoaded: Boolean = false

  /** Call before any `IsCollection`/`IsMap`/`IsOption`/`IsEither`/`IsValueType`/`CtorLikes` usage. Idempotent. */
  protected def ensureStandardExtensionsLoaded(): Unit =
    if (!standardExtensionsLoaded) {
      Environment.loadStandardExtensions().toEither match {
        case Right(_)     => standardExtensionsLoaded = true
        case Left(errors) =>
          // Behaves like Hearth's `.toMIO(allowFailures = false)`: a broken extension fails the expansion loudly.
          assertionFailed(
            ("Failed to load Hearth standard extensions:" +: errors.toVector.map(error => s"  ${error.getMessage}"))
              .mkString("\n")
          )
      }
    }
}
