package io.scalaland.chimney.internal.compiletime

private[compiletime] trait TypesPlatform extends Types { this: DefinitionsPlatform =>

  import c.universe.{internal as _, Transformer as _, *}

  protected type Tagged[U] = { type Tag = U }
  protected type @@[T, U] = T & Tagged[U]

  final override protected type Type[T] = c.Type @@ T
  protected object typeUtils {
    def fromUntyped[T](untyped: c.Type): Type[T] = untyped.asInstanceOf[Type[T]]
    def fromWeak[T: WeakTypeTag]: Type[T] = fromUntyped(weakTypeOf[T])
    def fromWeakTC[Unswapped: WeakTypeTag, T](args: c.Type*): Type[T] = fromUntyped {
      val ee = weakTypeOf[Unswapped].etaExpand
      // $COVERAGE-OFF$
      if (ee.typeParams.size != args.size) {
        val een = ee.typeParams.size
        val argsn = args.size
        c.abort(c.enclosingPosition, s"Type $ee has different arity ($een) than applied to applyTypeArgs ($argsn)!")
      }
      // $COVERAGE-ON$
      ee.finalResultType.substituteTypes(ee.typeParams, args.toList)
    }

    object fromWeakConversion {
      // convert WeakTypeTag[T] to Type[T] automatically
      implicit def typeFromWeak[T: WeakTypeTag]: Type[T] = typeUtils.fromWeak
    }
  }

  object Type extends TypeModule {
    import typeUtils.*
    val Any: Type[Any] = fromWeak[Any]
    val Int: Type[Int] = fromWeak[Int]
    val Unit: Type[Unit] = fromWeak[Unit]

    def Function1[From: Type, To: Type]: Type[From => To] = fromWeakTC[? => ?, From => To](Type[From], Type[To])

    object Array extends ArrayModule {
      def apply[T: Type]: Type[Array[T]] = fromWeakTC[Array[?], Array[T]](Type[T])
    }

    def Option[T: Type]: Type[Option[T]] = fromWeakTC[Option[?], Option[T]](Type[T])

    def Either[L: Type, R: Type]: Type[Either[L, R]] = fromWeakTC[Either[?, ?], Either[L, R]](Type[L], Type[R])

    def isSubtypeOf[S, T](S: Type[S], T: Type[T]): Boolean = S.<:<(T)
    def isSameAs[S, T](S: Type[S], T: Type[T]): Boolean = S.=:=(T)
  }

//  implicit class UntypedTypeOps(private val tpe: c.Type) {
//
//    /** Assumes that this `tpe` is String singleton type and extracts its value */
//    def asStringSingletonType: String = tpe
//      .asInstanceOf[scala.reflect.internal.Types#UniqueConstantType]
//      .value
//      .value
//      .asInstanceOf[String]
//  }
}
