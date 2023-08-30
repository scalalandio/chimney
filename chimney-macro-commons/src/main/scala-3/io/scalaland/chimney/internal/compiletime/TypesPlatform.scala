package io.scalaland.chimney.internal.compiletime

import scala.quoted
import scala.collection.compat.Factory

private[compiletime] trait TypesPlatform extends Types { this: DefinitionsPlatform =>

  import quotes.*, quotes.reflect.*

  final override protected type Type[T] = quoted.Type[T]
  protected object Type extends TypeModule {

    object platformSpecific {

      def fromUntyped[A](untyped: TypeRepr): Type[A] = untyped.asType.asInstanceOf[Type[A]]

      /** Applies type arguments obtained from tpe to the type parameters in method's parameters' types */
      // TODO: assumes each parameter list is made completely out of types OR completely out of values
      def paramListsOf(tpe: TypeRepr, method: Symbol): List[List[Symbol]] =
        method.paramSymss.filterNot(_.exists(_.isType))

      /** Applies type arguments obtained from tpe to the type parameters in method's return type */
      def returnTypeOf[A](tpe: TypeRepr, method: Symbol): Type[A] = tpe.memberType(method).widenByName match {
        case lambda: LambdaType => fromUntyped[A](lambda.resType)
        case out                => fromUntyped[A](out)
      }

      /** What is the type of each method parameter */
      def paramsWithTypes(tpe: TypeRepr, method: Symbol, isConstructor: Boolean): Map[String, TypeRepr] =
        // constructor methods still have to have their type parameters manually applied,
        // even if we know the exact type of their class
        val appliedIfNecessary =
          if tpe.typeArgs.isEmpty && isConstructor then tpe.memberType(method)
          else tpe.memberType(method).appliedTo(tpe.typeArgs)
        appliedIfNecessary match {
          // monomorphic
          case MethodType(names, types, _) => names.zip(types).toMap
          // polymorphic
          case PolyType(_, _, MethodType(names, types, AppliedType(_, typeRefs))) =>
            val typeArgumentByAlias = typeRefs.zip(tpe.typeArgs).toMap
            val typeArgumentByName: Map[String, TypeRepr] =
              names
                .zip(types)
                .toMap
                .view
                .mapValues { tpe =>
                  typeArgumentByAlias.getOrElse(tpe, tpe)
                }
                .toMap
            typeArgumentByName
          // unknown
          case out =>
            assertionFailed(
              s"Constructor of ${Type.prettyPrint(fromUntyped[Any](tpe))} has unrecognized/unsupported format of type: $out"
            )
        }

      /** Applies type arguments from supertype to subtype if there are any */
      def subtypeTypeOf[A: Type](subtype: Symbol): ?<[A] =
        subtype.primaryConstructor.paramSymss match {
          // subtype takes type parameters
          case typeParamSymbols :: _ if typeParamSymbols.exists(_.isType) =>
            // we have to figure how subtypes type params map to parent type params
            val appliedTypeByParam: Map[String, TypeRepr] =
              subtype.typeRef
                .baseType(TypeRepr.of[A].typeSymbol)
                .typeArgs
                .map(_.typeSymbol.name)
                .zip(TypeRepr.of[A].typeArgs)
                .toMap
            // TODO: some better error message if child has an extra type param that doesn't come from the parent
            val typeParamReprs: List[TypeRepr] = typeParamSymbols.map(_.name).map(appliedTypeByParam)
            fromUntyped[A](subtype.typeRef.appliedTo(typeParamReprs)).as_?<[A]
          // subtype is monomorphic
          case _ =>
            fromUntyped[A](subtype.typeRef).as_?<[A]
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
    val String: Type[String] = quoted.Type.of[String]

    def Tuple2[A: Type, B: Type]: Type[(A, B)] = quoted.Type.of[(A, B)]

    object Tuple2 extends Tuple2Module {
      def apply[A: Type, B: Type]: Type[(A, B)] = quoted.Type.of[(A, B)]
      def unapply[A](A: Type[A]): Option[(??, ??)] = A match {
        case '[(innerA, innerB)] => Some(Type[innerA].as_?? -> Type[innerB].as_??)
        case _                   => scala.None
      }
    }

    def Function1[A: Type, B: Type]: Type[A => B] = quoted.Type.of[A => B]
    def Function2[A: Type, B: Type, C: Type]: Type[(A, B) => C] = quoted.Type.of[(A, B) => C]

    object Array extends ArrayModule {
      def apply[A: Type]: Type[Array[A]] = quoted.Type.of[Array[A]]
      def unapply[A](A: Type[A]): Option[??] = A match {
        case '[Array[inner]] => Some(Type[inner].as_??)
        case _               => scala.None
      }
    }

    object Option extends OptionModule {

      def apply[A: Type]: Type[Option[A]] = quoted.Type.of[Option[A]]
      def unapply[A](A: Type[A]): Option[??] = A match {
        case '[Option[inner]] => Some(Type[inner].as_??)
        case _                => scala.None
      }

      val None: Type[scala.None.type] = quoted.Type.of[scala.None.type]
    }

    object Either extends EitherModule {
      def apply[L: Type, R: Type]: Type[Either[L, R]] = quoted.Type.of[Either[L, R]]
      def unapply[A](A: Type[A]): Option[(??, ??)] = A match {
        case '[Either[innerL, innerR]] => Some(Type[innerL].as_?? -> Type[innerR].as_??)
        case _                         => scala.None
      }

      object Left extends LeftModule {
        def apply[L: Type, R: Type]: Type[Left[L, R]] = quoted.Type.of[Left[L, R]]
        def unapply[A](A: Type[A]): Option[(??, ??)] = A match {
          case '[Left[innerL, innerR]] => Some(Type[innerL].as_?? -> Type[innerR].as_??)
          case _                       => scala.None
        }
      }
      object Right extends RightModule {
        def apply[L: Type, R: Type]: Type[Right[L, R]] = quoted.Type.of[Right[L, R]]
        def unapply[A](A: Type[A]): Option[(??, ??)] = A match {
          case '[Right[innerL, innerR]] => Some(Type[innerL].as_?? -> Type[innerR].as_??)
          case _                        => scala.None
        }
      }
    }

    object Iterable extends IterableModule {
      def apply[A: Type]: Type[Iterable[A]] = quoted.Type.of[Iterable[A]]
      def unapply[A](A: Type[A]): Option[??] = A match {
        case '[Iterable[inner]] => Some(Type[inner].as_??)
        case _                  => scala.None
      }
    }

    object Map extends MapModule {
      def apply[K: Type, V: Type]: Type[Map[K, V]] = quoted.Type.of[Map[K, V]]
      def unapply[A](A: Type[A]): Option[(??, ??)] = A match {
        case '[Map[innerK, innerV]] => Some(Type[innerK].as_?? -> Type[innerV].as_??)
        case _                      => scala.None
      }
    }

    object Iterator extends IteratorModule {
      def apply[A: Type]: Type[Iterator[A]] = quoted.Type.of[Iterator[A]]
      def unapply[A](A: Type[A]): Option[(??)] = A match {
        case '[Iterator[inner]] => Some(Type[inner].as_??)
        case _                  => scala.None
      }
    }

    def Factory[A: Type, C: Type]: Type[Factory[A, C]] = quoted.Type.of[Factory[A, C]]

    def extractStringSingleton[S <: String](S: Type[S]): String = quoted.Type.valueOfConstant[S](using S) match {
      case Some(str) => str
      case None      => assertionFailed(s"Invalid string literal type: ${prettyPrint(S)}")
    }

    def isTuple[A](A: Type[A]): Boolean = TypeRepr.of(using A).typeSymbol.fullName.startsWith("scala.Tuple")

    def isSubtypeOf[A, B](A: Type[A], B: Type[B]): Boolean = TypeRepr.of(using A) <:< TypeRepr.of(using B)
    def isSameAs[A, B](A: Type[A], B: Type[B]): Boolean = TypeRepr.of(using A) =:= TypeRepr.of(using B)

    def prettyPrint[T: Type]: String = {
      val repr = TypeRepr.of[T]

      scala.util
        .Try {
          val symbolFullName = (repr.typeSymbol.fullName: String).replaceAll("\\$", "")
          val colorlessReprName = repr.dealias.show(using Printer.TypeReprCode)
          val colorfulReprName = repr.dealias.show(using Printer.TypeReprAnsiCode)

          // workaround for local classes skipping on package and enclosing object name
          if symbolFullName != colorlessReprName && symbolFullName.endsWith("__" + colorlessReprName) then
            symbolFullName.substring(0, symbolFullName.length - 2 - colorlessReprName.length) + colorfulReprName
          else if symbolFullName != colorlessReprName && symbolFullName.endsWith("_" + colorlessReprName) then
            symbolFullName.substring(0, symbolFullName.length - 1 - colorlessReprName.length) + colorfulReprName
          else colorfulReprName
        }
        .getOrElse(repr.toString)
    }
  }
}
