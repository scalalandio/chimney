package io.scalaland

import io.scalaland.chimney.dsl.TransformerDefinition
import io.scalaland.chimney.internal.TransformerCfg
import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.language.experimental.macros

package object chimney {
  type Id[A] = A

  type Transformer[From, To] = TransformerF[Id, From, To]

  object Transformer {
    def derive[From, To]: Transformer[From, To] =
      macro ChimneyBlackboxMacros.deriveTransformerImplId[From, To]

    def define[From, To]: TransformerDefinition[Id, From, To, TransformerCfg.Empty] =
      new TransformerDefinition[Id, From, To, TransformerCfg.Empty](Map.empty, Map.empty)
  }

  implicit val idContext: TransformationContext[Id] = new TransformationContext[Id] {
    override def pure[A](x: A): Id[A] = x

    override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

    override def product[A, B](fa: Id[A], fb: Id[B]): Id[(A, B)] = (fa, fb)

    override def addPrefix[A](fa: Id[A], prefix: String): Id[A] = fa

    // only if unsafe cfg params, like enableUnsafeOptions
    override def error[A](message: String): Id[A] =
      throw TransformationException(message)
  }
}
