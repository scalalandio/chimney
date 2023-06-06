package io.scalaland.chimney.internal.compiletime

private[compiletime] trait TypesPlatform extends Types { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  protected type Tagged[Tag_] = { type Tag = Tag_ }
  protected type @@[A, Tag] = A & Tagged[Tag]

  final override protected type Type[A] = c.Type @@ A
  protected object Type extends TypeModule {

    object platformSpecific {

      def fromUntyped[A](untyped: c.Type): Type[A] = untyped.asInstanceOf[Type[A]]
      def fromWeak[A: WeakTypeTag]: Type[A] = fromUntyped(weakTypeOf[A])
      def fromWeakTypeConstructor[Unswapped: WeakTypeTag, A](args: c.Type*): Type[A] = fromUntyped {
        // $COVERAGE-OFF$
        val ee = weakTypeOf[Unswapped].etaExpand
        if (ee.typeParams.isEmpty || args.isEmpty) {
          assertionFailed(
            s"fromWeakTC should be used only to apply type paramerers to type constructors, got $ee and $args!"
          )
        } else if (ee.typeParams.size != args.size) {
          val een = ee.typeParams.size
          val argsn = args.size
          assertionFailed(s"Type $ee has different arity ($een) than applied to applyTypeArgs ($argsn)!")
        } else if (args.contains(null)) {
          assertionFailed("One of type parameters to apply was null!")
        } else {
          ee.finalResultType.substituteTypes(ee.typeParams, args.toList)
        }
        // $COVERAGE-ON$
      }

      /** Applies type arguments obtained from tpe to the type parameters in method's parameters' types */
      def paramListsOf(tpe: c.Type, method: c.Symbol): List[List[c.universe.Symbol]] =
        method.asMethod.typeSignatureIn(tpe).paramLists

      /** Applies type arguments obtained from tpe to the type parameters in method's return type */
      def returnTypeOf(tpe: c.Type, method: c.Symbol): c.universe.Type = method.typeSignatureIn(tpe).finalResultType

      object fromWeakConversion {
        // convert WeakTypeTag[T] to Type[T] automatically
        implicit def typeFromWeak[T: WeakTypeTag]: Type[T] = fromWeak
      }

      object toWeakConversion {

        // Required because:
        // - c.Expr[A] needs WeakTypeTag[A]
        // - if we used sth like A: Type WeakTypeTag would resolve to macro method's A rather than content of Type[A]
        implicit def weakFromType[A: Type]: WeakTypeTag[A] = c.WeakTypeTag(Type[A])
      }
    }

    import platformSpecific.{fromUntyped, fromWeak, fromWeakTypeConstructor}

    val Nothing: Type[Nothing] = fromWeak[Nothing]
    val Any: Type[Any] = fromWeak[Any]
    val AnyVal: Type[AnyVal] = fromWeak[AnyVal]
    val Boolean: Type[Boolean] = fromWeak[Boolean]
    val Byte: Type[Byte] = fromWeak[Byte]
    val Char: Type[Char] = fromWeak[Char]
    val Short: Type[Short] = fromWeak[Short]
    val Int: Type[Int] = fromWeak[Int]
    val Long: Type[Long] = fromWeak[Long]
    val Float: Type[Float] = fromWeak[Float]
    val Double: Type[Double] = fromWeak[Double]
    val Unit: Type[Unit] = fromWeak[Unit]

    def Tuple2[A: Type, B: Type]: Type[(A, B)] =
      fromWeakTypeConstructor[(?, ?), (A, B)](Type[A], Type[B])

    def Function1[A: Type, B: Type]: Type[A => B] =
      fromWeakTypeConstructor[? => ?, A => B](Type[A], Type[B])
    def Function2[A: Type, B: Type, C: Type]: Type[(A, B) => C] =
      fromWeakTypeConstructor[(?, ?) => ?, (A, B) => C](Type[A], Type[B], Type[C])

    object Array extends ArrayModule {
      def apply[A: Type]: Type[Array[A]] = fromWeakTypeConstructor[Array[?], Array[A]](Type[A])
      def unapply[A](tpe: Type[A]): Option[ExistentialType] =
        // Array is invariant so we cannot check with Array[Any] <:< A
        if (fromWeak[Array[?]].typeConstructor <:< tpe.typeConstructor)
          Some(fromUntyped(tpe.typeArgs.head).asExistential)
        else scala.None
    }

    object Option extends OptionModule {
      def apply[A: Type]: Type[Option[A]] = fromWeakTypeConstructor[Option[?], Option[A]](Type[A])
      def unapply[A](tpe: Type[A]): Option[ExistentialType] =
        // None has no type parameters, so we need getOrElse(Nothing)
        if (apply[Any](Any) <:< tpe)
          Some(
            tpe.typeArgs.headOption.fold[ExistentialType](ExistentialType(Nothing))(inner =>
              fromUntyped(inner).asExistential
            )
          )
        else scala.None

      val None: Type[scala.None.type] = fromWeak[scala.None.type]
    }

    object Either extends EitherModule {
      def apply[L: Type, R: Type]: Type[Either[L, R]] =
        fromWeakTypeConstructor[Either[?, ?], Either[L, R]](Type[L], Type[R])
      def unapply[A](tpe: Type[A]): Option[(ExistentialType, ExistentialType)] =
        if (apply[Any, Any](Any, Any) <:< tpe)
          Some(fromUntyped(tpe.typeArgs.head).asExistential -> fromUntyped(tpe.typeArgs.tail.head).asExistential)
        else scala.None

      object Left extends LeftModule {
        def apply[L: Type, R: Type]: Type[Left[L, R]] =
          fromWeakTypeConstructor[Left[?, ?], Left[L, R]](Type[L], Type[R])
        def unapply[A](tpe: Type[A]): Option[(ExistentialType, ExistentialType)] =
          if (apply[Any, Any](Any, Any) <:< tpe)
            Some(fromUntyped(tpe.typeArgs.head).asExistential -> fromUntyped(tpe.typeArgs.tail.head).asExistential)
          else scala.None
      }
      object Right extends RightModule {
        def apply[L: Type, R: Type]: Type[Right[L, R]] =
          fromWeakTypeConstructor[Right[?, ?], Right[L, R]](Type[L], Type[R])
        def unapply[A](tpe: Type[A]): Option[(ExistentialType, ExistentialType)] =
          if (apply[Any, Any](Any, Any) <:< tpe)
            Some(fromUntyped(tpe.typeArgs.head).asExistential -> fromUntyped(tpe.typeArgs.tail.head).asExistential)
          else scala.None
      }
    }

    object Iterable extends IterableModule {
      def apply[A: Type]: Type[Iterable[A]] = fromWeakTypeConstructor[Iterable[?], Iterable[A]](Type[A])
      def unapply[A](tpe: Type[A]): Option[ExistentialType] =
        if (apply[Any](Any) <:< tpe) Some(fromUntyped(tpe.typeArgs.head).asExistential)
        else scala.None
    }

    object Map extends MapModule {
      def apply[K: Type, V: Type]: Type[Map[K, V]] =
        fromWeakTypeConstructor[Map[?, ?], Map[K, V]](Type[K], Type[V])
      def unapply[A](tpe: Type[A]): Option[(ExistentialType, ExistentialType)] =
        if (apply[Any, Any](Any, Any) <:< tpe)
          Some(fromUntyped(tpe.typeArgs.head).asExistential -> fromUntyped(tpe.typeArgs.tail.head).asExistential)
        else scala.None
    }

    def isSubtypeOf[A, B](S: Type[A], T: Type[B]): Boolean = S.<:<(T)
    def isSameAs[A, B](S: Type[A], T: Type[B]): Boolean = S.=:=(T)

    def isSealed[A](A: Type[A]): Boolean = A.typeSymbol.asClass.isSealed

    def prettyPrint[A: Type]: String = Type[A].toString
  }
}
