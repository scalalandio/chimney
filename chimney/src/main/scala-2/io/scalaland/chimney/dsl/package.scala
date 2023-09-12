package io.scalaland.chimney

import scala.util.Try

import scala.language.implicitConversions

/** Main object to import in order to use Chimney's features
  *
  * @since 0.1.0
  */
package object dsl {

  // export inlined.*

  implicit def TransformationOps[From](from: From): inlined.TransformationOps[From] =
    inlined.TransformationOps(from)

  implicit def PartialTransformationOps[From](from: From): inlined.PartialTransformationOps[From] =
    inlined.PartialTransformationOps(from)

  implicit def PatchingOps[A](obj: A): inlined.PatchingOps[A] =
    inlined.PatchingOps(obj)

  // export syntax.*

  implicit def TransformerOps[From](from: From): syntax.TransformerOps[From] =
    syntax.TransformerOps(from)

  implicit def PartialTransformerOps[From](from: From): syntax.PartialTransformerOps[From] =
    syntax.PartialTransformerOps(from)

  implicit def PatcherOps[A](obj: A): syntax.PatcherOps[A] =
    syntax.PatcherOps(obj)

  implicit def OptionPartialTransformerOps[A](option: Option[A]): syntax.OptionPartialTransformerOps[A] =
    syntax.OptionPartialTransformerOps(option)

  implicit def EitherStringPartialTransformerOps[A](
      either: Either[String, A]
  ): syntax.EitherStringPartialTransformerOps[A] =
    syntax.EitherStringPartialTransformerOps(either)

  implicit def TryPartialTransformerOps[A](`try`: Try[A]): syntax.TryPartialTransformerOps[A] =
    syntax.TryPartialTransformerOps(`try`)
}
