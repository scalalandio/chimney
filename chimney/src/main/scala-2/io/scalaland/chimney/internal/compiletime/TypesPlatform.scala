package io.scalaland.chimney.internal.compiletime

private[compiletime] trait TypesPlatform extends Types { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  protected type Tagged[Tag_] = { type Tag = Tag_ }
  protected type @@[A, Tag] = A & Tagged[Tag]

  final override protected type Type[A] = c.Type @@ A
  protected object typeUtils {
    def fromUntyped[A](untyped: c.Type): Type[A] = untyped.asInstanceOf[Type[A]]
    def fromWeak[A: WeakTypeTag]: Type[A] = fromUntyped(weakTypeOf[A])
    def fromWeakTC[Unswapped: WeakTypeTag, A](args: c.Type*): Type[A] = fromUntyped {
      // $COVERAGE-OFF$
      val ee = weakTypeOf[Unswapped].etaExpand
      if (ee.typeParams.isEmpty || args.isEmpty) {
        c.abort(
          c.enclosingPosition,
          s"fromWeakTC should be used only to apply type paramerers to type constructors, got $ee and $args!"
        )
      } else if (ee.typeParams.size != args.size) {
        val een = ee.typeParams.size
        val argsn = args.size
        c.abort(c.enclosingPosition, s"Type $ee has different arity ($een) than applied to applyTypeArgs ($argsn)!")
      } else if (args.exists(_ == null)) {
        c.abort(c.enclosingPosition, "One of type parameters to apply was null!")
      } else {
        ee.finalResultType.substituteTypes(ee.typeParams, args.toList)
      }
      // $COVERAGE-ON$
    }

    object fromWeakConversion {
      // convert WeakTypeTag[T] to Type[T] automatically
      implicit def typeFromWeak[T: WeakTypeTag]: Type[T] = typeUtils.fromWeak
    }
  }

  object Type extends TypeModule {
    import typeUtils.*

    val Nothing: Type[Nothing] = fromWeak[Nothing]
    val Any: Type[Any] = fromWeak[Any]
    val Boolean: Type[Boolean] = fromWeak[Boolean]
    val Int: Type[Int] = fromWeak[Int]
    val Unit: Type[Unit] = fromWeak[Unit]

    def Function1[From: Type, To: Type]: Type[From => To] = fromWeakTC[? => ?, From => To](Type[From], Type[To])

    object Array extends ArrayModule {
      def apply[A: Type]: Type[Array[A]] = fromWeakTC[Array[?], Array[A]](Type[A])
    }

    object Option extends OptionModule {

      def apply[A: Type]: Type[Option[A]] = fromWeakTC[Option[?], Option[A]](Type[A])
      def unapply[A](tpe: Type[A]): Option[ComputedType] =
        // None has no type parameters, so we need getOrElse(Nothing)
        if (apply[Any](Any) <:< tpe)
          Some(
            tpe.typeArgs.headOption.fold[ComputedType](ComputedType(Nothing))(inner =>
              ComputedType(typeUtils.fromUntyped(inner))
            )
          )
        else scala.None

      val None: Type[scala.None.type] = fromWeak[scala.None.type]
    }

    def Either[L: Type, R: Type]: Type[Either[L, R]] = fromWeakTC[Either[?, ?], Either[L, R]](Type[L], Type[R])

    def isSubtypeOf[A, B](S: Type[A], T: Type[B]): Boolean = S.<:<(T)
    def isSameAs[A, B](S: Type[A], T: Type[B]): Boolean = S.=:=(T)

    def prettyPrint[A: Type]: String = Type[A].toString
  }
}
