package io.scalaland.chimney.internal.compiletime

import io.scalaland.chimney.dsl as dsls
import io.scalaland.chimney.internal
import io.scalaland.chimney.{partial, PartialTransformer, Patcher, Transformer}

import scala.quoted

private[compiletime] trait TypesPlatform extends Types { this: DefinitionsPlatform =>

  import quotes.*
  import quotes.reflect.*

  final override type Type[T] = quoted.Type[T]

  protected object typeUtils {
    def fromTC[Unswapped <: AnyKind: quoted.Type, T](args: Type[?]*): Type[T] = {
      val U = TypeRepr.of[Unswapped]
      // $COVERAGE-OFF$
      if U.typeArgs.size != args.size then {
        val een = U.typeArgs.size
        val argsn = args.size
        report.errorAndAbort(s"Type ${U.show} has different arity ($een) than applied to applyTypeArgs ($argsn)!")
      }
      // $COVERAGE-ON$
      U.appliedTo(args.map(t => TypeRepr.of(using t)).toList).asType.asInstanceOf[Type[T]]
    }
  }

  object Type extends TypeModule {
    import typeUtils.*

    val Nothing: Type[Nothing] = quoted.Type.of[Nothing]
    val Any: Type[Any] = quoted.Type.of[Any]
    val Boolean: Type[Boolean] = quoted.Type.of[Boolean]
    val Int: Type[Int] = quoted.Type.of[Int]
    val Unit: Type[Unit] = quoted.Type.of[Unit]

    def Function1[From: Type, To: Type]: Type[From => To] = fromTC[* => *, From => To](Type[From], Type[To])

    object Array extends ArrayModule {
      def apply[T: Type]: Type[Array[T]] = fromTC[Array[*], Array[T]](Type[T])
    }

    def Option[T: Type]: Type[Option[T]] = fromTC[Option[*], Option[T]](Type[T])
    def Either[L: Type, R: Type]: Type[Either[L, R]] = fromTC[Either[*, *], Either[L, R]](Type[L], Type[R])

    def isSubtypeOf[S, T](S: Type[S], T: Type[T]): Boolean = TypeRepr.of(using S) <:< TypeRepr.of(using T)
    def isSameAs[S, T](S: Type[S], T: Type[T]): Boolean = TypeRepr.of(using S) =:= TypeRepr.of(using T)

    def prettyPrint[T: Type]: String = {
      val repr = TypeRepr.of[T]
      scala.util.Try(repr.dealias.show(using Printer.TypeReprAnsiCode)).getOrElse(repr.toString)
    }
  }
}
