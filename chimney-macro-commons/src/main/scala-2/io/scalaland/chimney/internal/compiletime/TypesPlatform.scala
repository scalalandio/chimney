package io.scalaland.chimney.internal.compiletime

import scala.collection.compat.Factory

private[compiletime] trait TypesPlatform extends Types { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  final override protected type Type[A] = c.WeakTypeTag[A]
  object Type extends TypeModule {

    object platformSpecific {

      def fromUntyped[A](untyped: c.Type): Type[A] = c.WeakTypeTag(untyped)

      /** Applies type arguments obtained from tpe to the type parameters in method's parameters' types */
      def paramListsOf(tpe: c.Type, method: c.Symbol): List[List[c.universe.Symbol]] =
        method.asMethod.typeSignatureIn(tpe).paramLists

      /** Applies type arguments obtained from tpe to the type parameters in method's return type */
      def returnTypeOf(tpe: c.Type, method: c.Symbol): c.Type = method.typeSignatureIn(tpe).finalResultType

      /** What is the type of each method parameter */
      def paramsWithTypes(tpe: c.Type, method: c.Symbol): Map[String, c.universe.Type] = (for {
        params <- paramListsOf(tpe, method)
        param <- params
      } yield param.name.decodedName.toString -> param.typeSignatureIn(tpe)).toMap

      /** Applies type arguments from supertype to subtype if there are any */
      def subtypeTypeOf[A: Type](subtype: TypeSymbol): ?<[A] = {
        subtype.typeSignature // Workaround for <https://issues.scala-lang.org/browse/SI-7755>

        val sEta = subtype.toType.etaExpand
        fromUntyped[A](
          sEta.finalResultType.substituteTypes(
            sEta.baseType(Type[A].tpe.typeSymbol).typeArgs.map(_.typeSymbol),
            Type[A].tpe.typeArgs
          )
        ).as_?<[A]
      }

      implicit class TypeCtorOps[A](private val A: Type[A]) {

        def isCtor[B: Type]: Boolean = weakTypeOf(A).typeConstructor <:< weakTypeOf[B].typeConstructor

        def param(idx: Int): ?? = fromUntyped(A.tpe.typeArgs(idx)).as_??
        def param_>[L](idx: Int): ?>[L] = fromUntyped[L](A.tpe.typeArgs(idx)).as_?>[L]
        def param_<[U](idx: Int): ?<[U] = fromUntyped[U](A.tpe.typeArgs(idx)).as_?<[U]
        def param_>?<[L, U >: L](idx: Int): L >?< U = fromUntyped[L](A.tpe.typeArgs(idx)).as_>?<[L, U]
      }
    }

    import platformSpecific.*

    val Nothing: Type[Nothing] = weakTypeTag[Nothing]
    val Null: Type[Null] = weakTypeTag[Null]
    val Any: Type[Any] = weakTypeTag[Any]
    val AnyVal: Type[AnyVal] = weakTypeTag[AnyVal]
    val Boolean: Type[Boolean] = weakTypeTag[Boolean]
    val Byte: Type[Byte] = weakTypeTag[Byte]
    val Char: Type[Char] = weakTypeTag[Char]
    val Short: Type[Short] = weakTypeTag[Short]
    val Int: Type[Int] = weakTypeTag[Int]
    val Long: Type[Long] = weakTypeTag[Long]
    val Float: Type[Float] = weakTypeTag[Float]
    val Double: Type[Double] = weakTypeTag[Double]
    val Unit: Type[Unit] = weakTypeTag[Unit]
    val String: Type[String] = weakTypeTag[String]

    object Tuple2 extends Tuple2Module {
      def apply[A: Type, B: Type]: Type[(A, B)] = weakTypeTag[(A, B)]
      def unapply[A](A: Type[A]): Option[(??, ??)] =
        if (A.isCtor[(?, ?)]) Some(A.param(0) -> A.param(1))
        else scala.None
    }

    def Function1[A: Type, B: Type]: Type[A => B] = weakTypeTag[A => B]
    def Function2[A: Type, B: Type, C: Type]: Type[(A, B) => C] = weakTypeTag[(A, B) => C]

    object Array extends ArrayModule {
      def apply[A: Type]: Type[Array[A]] = weakTypeTag[Array[A]]
      def unapply[A](A: Type[A]): Option[??] =
        if (A.isCtor[Array[?]]) Some(A.param(0))
        else scala.None
    }

    object Option extends OptionModule {
      def apply[A: Type]: Type[Option[A]] = weakTypeTag[Option[A]]
      def unapply[A](A: Type[A]): Option[??] =
        if (A <:< None) Some(ExistentialType(Nothing))
        else if (A.isCtor[Option[?]]) Some(A.param(0))
        else scala.None

      val None: Type[scala.None.type] = weakTypeTag[scala.None.type]
    }

    object Either extends EitherModule {
      def apply[L: Type, R: Type]: Type[Either[L, R]] = weakTypeTag[Either[L, R]]
      def unapply[A](A: Type[A]): Option[(??, ??)] =
        if (A.isCtor[Either[?, ?]]) Some(A.param(0) -> A.param(1))
        else scala.None

      object Left extends LeftModule {
        def apply[L: Type, R: Type]: Type[Left[L, R]] = weakTypeTag[Left[L, R]]
        def unapply[A](A: Type[A]): Option[(??, ??)] =
          if (A.isCtor[Left[?, ?]]) Some(A.param(0) -> A.param(1))
          else scala.None
      }
      object Right extends RightModule {
        def apply[L: Type, R: Type]: Type[Right[L, R]] = weakTypeTag[Right[L, R]]
        def unapply[A](A: Type[A]): Option[(??, ??)] =
          if (A.isCtor[Right[?, ?]]) Some(A.param(0) -> A.param(1))
          else scala.None
      }
    }

    object Iterable extends IterableModule {
      def apply[A: Type]: Type[Iterable[A]] = weakTypeTag[Iterable[A]]
      def unapply[A](A: Type[A]): Option[??] =
        if (A.isCtor[Iterable[?]]) Some(A.param(0))
        else scala.None
    }

    object Map extends MapModule {
      def apply[K: Type, V: Type]: Type[Map[K, V]] = weakTypeTag[Map[K, V]]
      def unapply[A](A: Type[A]): Option[(??, ??)] =
        if (A.isCtor[Map[?, ?]]) Some(A.param(0) -> A.param(1))
        else scala.None
    }

    object Iterator extends IteratorModule {
      def apply[A: Type]: Type[Iterator[A]] = weakTypeTag[Iterator[A]]
      def unapply[A](A: Type[A]): Option[(??)] =
        if (A.isCtor[Iterator[?]]) Some(A.param(0))
        else scala.None
    }

    def Factory[A: Type, C: Type]: Type[Factory[A, C]] = weakTypeTag[Factory[A, C]]

    def extractStringSingleton[S <: String](S: Type[S]): String = scala.util
      .Try(
        S.tpe
          .asInstanceOf[scala.reflect.internal.Types#UniqueConstantType]
          .value
          .value
          .asInstanceOf[String]
      )
      .getOrElse(assertionFailed(s"Invalid string literal type: ${prettyPrint(S)}"))

    def isTuple[A](A: Type[A]): Boolean = A.tpe.typeSymbol.fullName.startsWith("scala.Tuple")

    def isSubtypeOf[A, B](A: Type[A], B: Type[B]): Boolean = A.tpe <:< B.tpe
    def isSameAs[A, B](A: Type[A], B: Type[B]): Boolean = A.tpe =:= B.tpe

    def prettyPrint[A: Type]: String = {
      def helper(tpe: c.Type): String = {
        val tpes = tpe.typeArgs.map(helper)
        tpe.dealias.typeSymbol.fullName + (if (tpes.isEmpty) "" else s"[${tpes.mkString(", ")}]")
      }
      Console.MAGENTA + helper(Type[A].tpe) + Console.RESET
    }
  }
}
