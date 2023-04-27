package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Types {

  /** Platform-specific type representation (c.universe.Type in 2, scala.quoted.Type[A] in 3) */
  protected type Type[T]

  val Type: TypeModule
  trait TypeModule { this: Type.type =>
    def apply[T](implicit T: Type[T]): Type[T] = T

    val Any: Type[Any]
    val Boolean: Type[Boolean]
    val Int: Type[Int]
    val Unit: Type[Unit]

    def Function1[From: Type, To: Type]: Type[From => To]

    val Array: ArrayModule
    trait ArrayModule { this: Array.type =>
      def apply[T: Type]: Type[Array[T]]
      val Any: Type[Array[Any]] = apply(Type.Any)
    }

    def Option[T: Type]: Type[Option[T]]
    def Either[L: Type, R: Type]: Type[Either[L, R]]

    def isSubtypeOf[S, T](S: Type[S], T: Type[T]): Boolean
    def isSameAs[S, T](S: Type[S], T: Type[T]): Boolean

    def prettyPrint[T: Type]: String
  }

  implicit class TypeOps[T](private val tpe: Type[T]) {

    final def <:<[S](another: Type[S]): Boolean = Type.isSubtypeOf(tpe, another)
    final def =:=[S](another: Type[S]): Boolean = Type.isSameAs(tpe, another)
  }

  /** Used to erase the type of Type, while providing the utilities to still make it useful */
  type ComputedType = { type Underlying }

  object ComputedType {
    def apply[T](tpe: Type[T]): ComputedType = tpe.asInstanceOf[ComputedType]
  }

  implicit class ComputedTypeOps(val ct: ComputedType) {
    def Type: Type[ct.Underlying] = ct.asInstanceOf[Type[ct.Underlying]]
    def use[Out](thunk: Type[ct.Underlying] => Out): Out = thunk(Type)
  }
}
