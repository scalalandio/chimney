package io.scalaland.chimney.internal.compiletime

import scala.quoted
import scala.collection.compat.Factory

private[compiletime] trait TypesPlatform extends Types { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  final override protected type Type[T] = quoted.Type[T]
  protected object Type extends TypeModule {

    object platformSpecific {

      @scala.annotation.tailrec
      def returnTypeOf[A](typeRepr: TypeRepr): Type[A] = typeRepr.widenByName match {
        case MethodType(_, _, out) => returnTypeOf[A](out)
        case out                   => out.asType.asInstanceOf[Type[A]]
      }

      def resolveTypeArgsForMethodArguments(tpe: TypeRepr, method: Symbol): (Map[String, TypeRepr], List[TypeRepr]) =
        tpe.memberType(method) match {
          // monomorphic
          case MethodType(names, types, _) =>
            val typeArgs: List[TypeRepr] = Nil
            val typeArgumentByName: Map[String, TypeRepr] = names.zip(types).toMap
            typeArgumentByName -> typeArgs
          // polymorphic
          case PolyType(_, _, MethodType(names, types, AppliedType(_, typeRefs))) =>
            // TODO: check if types of constructor match types passed to tpe
            val typeArgs: List[TypeRepr] = tpe.typeArgs
            val typeArgumentByAlias = typeRefs.zip(typeArgs).toMap
            val typeArgumentByName: Map[String, TypeRepr] = names
              .zip(types)
              .toMap
              .view
              .mapValues { tpe =>
                typeArgumentByAlias.getOrElse(tpe, tpe)
              }
              .toMap
            typeArgumentByName -> typeArgs
          // unknown
          case tpe =>
            assertionFailed(
              s"Constructor of ${Type.prettyPrint(tpe.asType.asInstanceOf[Type[Any]])} has unrecognized/unsupported format of type: ${tpe}"
            )
        }
    }

    val Nothing: Type[Nothing] = quoted.Type.of[Nothing]
    val Null: Type[Null] = quoted.Type.of[Null]
    val Any: Type[Any] = quoted.Type.of[Any]
    val AnyVal: Type[AnyVal] = quoted.Type.of[AnyVal]
    val Boolean: Type[Boolean] = quoted.Type.of[Boolean]
    val Byte: Type[Byte] = quoted.Type.of[Byte]
    val Char: Type[Char] = quoted.Type.of[Char]
    val Short: Type[Short] = quoted.Type.of[Short]
    val Int: Type[Int] = quoted.Type.of[Int]
    val Long: Type[Long] = quoted.Type.of[Long]
    val Float: Type[Float] = quoted.Type.of[Float]
    val Double: Type[Double] = quoted.Type.of[Double]
    val Unit: Type[Unit] = quoted.Type.of[Unit]

    def Tuple2[A: Type, B: Type]: Type[(A, B)] = quoted.Type.of[(A, B)]

    def Function1[A: Type, B: Type]: Type[A => B] = quoted.Type.of[A => B]
    def Function2[A: Type, B: Type, C: Type]: Type[(A, B) => C] = quoted.Type.of[(A, B) => C]

    object Array extends ArrayModule {
      def apply[A: Type]: Type[Array[A]] = quoted.Type.of[Array[A]]
      def unapply[A](tpe: Type[A]): Option[ExistentialType] = tpe match {
        case '[Array[inner]] => Some(Type[inner].asExistential)
        case _               => scala.None
      }
    }

    object Option extends OptionModule {

      def apply[A: Type]: Type[Option[A]] = quoted.Type.of[Option[A]]
      def unapply[A](tpe: Type[A]): Option[ExistentialType] = tpe match {
        case '[Option[inner]] => Some(Type[inner].asExistential)
        case _                => scala.None
      }

      val None: Type[scala.None.type] = quoted.Type.of[scala.None.type]
    }

    object Either extends EitherModule {
      def apply[L: Type, R: Type]: Type[Either[L, R]] = quoted.Type.of[Either[L, R]]
      def unapply[A](tpe: Type[A]): Option[(ExistentialType, ExistentialType)] = tpe match {
        case '[Either[innerL, innerR]] => Some(Type[innerL].asExistential -> Type[innerR].asExistential)
        case _                         => scala.None
      }

      object Left extends LeftModule {
        def apply[L: Type, R: Type]: Type[Left[L, R]] = quoted.Type.of[Left[L, R]]
        def unapply[A](tpe: Type[A]): Option[(ExistentialType, ExistentialType)] = tpe match {
          case '[Left[innerL, innerR]] => Some(Type[innerL].asExistential -> Type[innerR].asExistential)
          case _                       => scala.None
        }
      }
      object Right extends RightModule {
        def apply[L: Type, R: Type]: Type[Right[L, R]] = quoted.Type.of[Right[L, R]]
        def unapply[A](tpe: Type[A]): Option[(ExistentialType, ExistentialType)] = tpe match {
          case '[Right[innerL, innerR]] => Some(Type[innerL].asExistential -> Type[innerR].asExistential)
          case _                        => scala.None
        }
      }
    }

    object Iterable extends IterableModule {
      def apply[A: Type]: Type[Iterable[A]] = quoted.Type.of[Iterable[A]]
      def unapply[A](tpe: Type[A]): Option[ExistentialType] = tpe match {
        case '[Iterable[inner]] => Some(Type[inner].asExistential)
        case _                  => scala.None
      }
    }

    object Map extends MapModule {
      def apply[K: Type, V: Type]: Type[Map[K, V]] = quoted.Type.of[Map[K, V]]
      def unapply[A](tpe: Type[A]): Option[(ExistentialType, ExistentialType)] = tpe match {
        case '[Map[innerK, innerV]] => Some(Type[innerK].asExistential -> Type[innerV].asExistential)
        case _                      => scala.None
      }
    }

    object Iterator extends IteratorModule {
      def apply[A: Type]: Type[Iterator[A]] = quoted.Type.of[Iterator[A]]
      def unapply[A](tpe: Type[A]): Option[(ExistentialType)] = tpe match {
        case '[Iterator[inner]] => Some(Type[inner].asExistential)
        case _                  => scala.None
      }
    }

    def Factory[A: Type, C: Type]: Type[Factory[A, C]] = quoted.Type.of[Factory[A, C]]

    def isTuple[A](A: Type[A]): Boolean = TypeRepr.of(using A).typeSymbol.fullName.startsWith("scala.Tuple")

    def isSubtypeOf[A, B](A: Type[A], B: Type[B]): Boolean = TypeRepr.of(using A) <:< TypeRepr.of(using B)
    def isSameAs[A, B](A: Type[A], B: Type[B]): Boolean = TypeRepr.of(using A) =:= TypeRepr.of(using B)

    def prettyPrint[T: Type]: String = {
      val repr = TypeRepr.of[T]
      scala.util.Try(repr.dealias.show(using Printer.TypeReprAnsiCode)).getOrElse(repr.toString)
    }
  }
}
