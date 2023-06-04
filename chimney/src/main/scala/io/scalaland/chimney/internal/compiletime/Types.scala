package io.scalaland.chimney.internal.compiletime

private[compiletime] trait Types {

  /** Platform-specific type representation (c.universe.Type in 2, scala.quoted.Type[A] in 3) */
  protected type Type[A]

  val Type: TypeModule
  trait TypeModule { this: Type.type =>
    def apply[A](implicit A: Type[A]): Type[A] = A

    val Nothing: Type[Nothing]
    val Any: Type[Any]
    val Boolean: Type[Boolean]
    val Int: Type[Int]
    val Unit: Type[Unit]

    def Tuple2[A: Type, B: Type]: Type[(A, B)]

    def Function1[A: Type, B: Type]: Type[A => B]
    def Function2[A: Type, B: Type, C: Type]: Type[(A, B) => C]

    val Array: ArrayModule
    trait ArrayModule { this: Array.type =>
      def apply[A: Type]: Type[Array[A]]
      val Any: Type[Array[Any]] = apply(Type.Any)
    }

    val Option: OptionModule
    trait OptionModule { this: Option.type =>

      def apply[A: Type]: Type[Option[A]]
      def unapply[A](tpe: Type[A]): Option[ComputedType]

      val None: Type[scala.None.type]
    }

    val Either: EitherModule
    trait EitherModule { this: Either.type =>
      def apply[L: Type, R: Type]: Type[Either[L, R]]
      def Left[L: Type, R: Type]: Type[Left[L, R]]
      def Right[L: Type, R: Type]: Type[Right[L, R]]
    }

    def isSubtypeOf[A, B](S: Type[A], T: Type[B]): Boolean
    def isSameAs[A, B](S: Type[A], T: Type[B]): Boolean

    def prettyPrint[A: Type]: String
  }

  implicit class TypeOps[A](private val tpe: Type[A]) {

    final def <:<[B](another: Type[B]): Boolean = Type.isSubtypeOf(tpe, another)
    final def =:=[B](another: Type[B]): Boolean = Type.isSameAs(tpe, another)

    final def isOption: Boolean = tpe <:< Type.Option(Type.Any)

    final def asComputed: ComputedType = ComputedType(tpe)
  }

  /** Used to erase the type of Type, while providing the utilities to still make it useful */
  type ComputedType = { type Underlying }

  object ComputedType {
    def apply[A](tpe: Type[A]): ComputedType = tpe.asInstanceOf[ComputedType]

    def prettyPrint(computedType: ComputedType): String = Type.prettyPrint(computedType.Type)

    def use[Out](ct: ComputedType)(thunk: Type[ct.Underlying] => Out): Out = thunk(ct.asInstanceOf[Type[ct.Underlying]])
  }

  implicit class ComputedTypeOps(val ct: ComputedType) {
    def Type: Type[ct.Underlying] = ct.asInstanceOf[Type[ct.Underlying]]
  }

  // you can import TypeImplicits.* in your shared code to avoid providing types manually, while avoiding conflicts with
  // implicit types seen in platform-specific scopes
  protected object TypeImplicits {

    implicit val NothingType: Type[Nothing] = Type.Nothing
    implicit val AnyType: Type[Any] = Type.Any
    implicit val BooleanType: Type[Boolean] = Type.Boolean
    implicit val IntType: Type[Int] = Type.Int
    implicit val UnitType: Type[Unit] = Type.Unit

    implicit def Tuple2Type[A: Type, B: Type]: Type[(A, B)] = Type.Tuple2[A, B]

    implicit def Function1Type[A: Type, B: Type]: Type[A => B] = Type.Function1[A, B]
    implicit def Function2Type[A: Type, B: Type, C: Type]: Type[(A, B) => C] = Type.Function2[A, B, C]

    implicit def ArrayType[A: Type]: Type[Array[A]] = Type.Array[A]
    implicit def OptionType[A: Type]: Type[Option[A]] = Type.Option[A]
    implicit val NoneType: Type[None.type] = Type.Option.None
    implicit def EitherType[L: Type, R: Type]: Type[Either[L, R]] = Type.Either[L, R]
    implicit def LeftType[L: Type, R: Type]: Type[Left[L, R]] = Type.Either.Left[L, R]
    implicit def RightType[L: Type, R: Type]: Type[Right[L, R]] = Type.Either.Right[L, R]
  }
}
