package io.scalaland.chimney

/** Marker used to enable derivation debug logging for every Chimney macro expansion that sees it in the implicit
  * scope.
  *
  * Do not instantiate it directly - `import io.scalaland.chimney.debug._` in the file you want to inspect (or enable
  * logging project-wide with `-Xmacro-settings:chimney.logDerivation=true`). Same idiom as Kindlings' per-module
  * `debug._` imports.
  *
  * @see
  *   [[https://chimney.readthedocs.io Chimney documentation - debugging chapter]]
  *
  * @since 2.0.0
  */
final class LogDerivation
