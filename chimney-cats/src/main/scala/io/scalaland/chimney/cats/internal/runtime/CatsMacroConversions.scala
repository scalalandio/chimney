package io.scalaland.chimney.cats.internal.runtime

import io.scalaland.chimney.partial

/** Runtime helpers referenced (fully qualified) from `CatsChimneyMacroExtension`'s `Expr.quote` bodies.
  *
  * Every actual Cats operation lives HERE, in normal (non-quoted) Scala, so the macro's quotes never have to reify a
  * Cats extension-ops conversion or a higher-kinded/phantom `AnyK1` type - both of which Scala 2 cross-quotes fail to
  * emit correctly. The quotes only splice already-derived values (source, element function, `Order`) into a call to one
  * of these methods and `asInstanceOf`-cast the result; the methods' type parameters are INFERRED from those value
  * arguments at the splice site, so no path-dependent existential (`elem.Underlying`) ever gets reified either.
  *
  * Higher-kinded instances are received erased: `cats.Traverse[AnyF]` (`AnyF[x] = Any`, so `map`/`traverseWithIndexM`
  * operate on `Any`) and `cats.arrow.FunctionK[cats.Id, cats.Id]` (`apply[A]` degenerates to `A => A`). This is sound
  * because at runtime the instances really are `Traverse[F]` / `F ~> G` and the values really are `F[A]`.
  */
object CatsMacroConversions {

  /** Erased unary type constructor: `AnyF[x] = Any` for every `x` (cross-compiles to Scala 2.13 and 3 unchanged). */
  type AnyF[x] = Any

  // --- general Traverse (handler #1) ---

  // The higher-kinded instances arrive as `Any`: the caller's summoned `Traverse[F]` / `F ~> G` cannot be spliced into
  // a `Traverse[AnyF]` / `FunctionK[Id, Id]` parameter position (Scala 2 keeps the tree's real `Traverse[List]` type,
  // and `Traverse` is invariant), so - like `src` - they are erased to `Any` at the boundary and cast back HERE.
  private def asTraverse(traverse: Any): _root_.cats.Traverse[AnyF] = traverse.asInstanceOf[_root_.cats.Traverse[AnyF]]
  private def asFunctionK(fk: Any): _root_.cats.arrow.FunctionK[_root_.cats.Id, _root_.cats.Id] =
    fk.asInstanceOf[_root_.cats.arrow.FunctionK[_root_.cats.Id, _root_.cats.Id]]

  def mapTraverse[A, B](traverse: Any, src: Any, f: A => B): Any =
    asTraverse(traverse).map[A, B](src)(f)

  def traversePartial[A, B](
      traverse: Any,
      src: Any,
      f: A => partial.Result[B]
  ): partial.Result[Any] =
    asTraverse(traverse).traverseWithIndexM[partial.Result, A, B](src) { (a, idx) =>
      f(a).prependErrorPath(partial.PathElement.Index(idx))
    }(io.scalaland.chimney.cats.catsCovariantForPartialResult)

  // --- Traverse + FunctionK (handler #4) ---

  def mapTraverseThroughFunctionK[A, B](
      traverse: Any,
      fk: Any,
      src: Any,
      f: A => B
  ): Any = asFunctionK(fk)(asTraverse(traverse).map[A, B](src)(f))

  def traversePartialThroughFunctionK[A, B](
      traverse: Any,
      fk: Any,
      src: Any,
      f: A => partial.Result[B]
  ): partial.Result[Any] = traversePartial[A, B](traverse, src, f).map(element => asFunctionK(fk)(element))

  // --- NonEmptyMap (handler #2) ---

  def mapNonEmptyMap[A, B, C, D](
      src: Any,
      f: ((A, B)) => (C, D),
      orderC: _root_.cats.kernel.Order[C]
  ): _root_.cats.data.NonEmptyMap[C, D] =
    src.asInstanceOf[_root_.cats.data.NonEmptyMap[A, B]].mapBoth((k, v) => f((k, v)))(orderC)

  def traverseNonEmptyMap[A, B, C, D](
      src: Any,
      f: ((A, B)) => partial.Result[(C, D)],
      failFast: Boolean,
      orderC: _root_.cats.kernel.Order[C]
  ): partial.Result[_root_.cats.data.NonEmptyMap[C, D]] =
    partial.Result
      .traverse[Seq[(C, D)], (A, B), (C, D)](
        src.asInstanceOf[_root_.cats.data.NonEmptyMap[A, B]].toSortedMap.iterator,
        f,
        failFast
      )
      .map(seq => _root_.cats.data.NonEmptyMap.of(seq.head, seq.tail*)(orderC))

  // --- NonEmptySet (handler #3) ---

  def mapNonEmptySet[A, B](src: Any, f: A => B, orderB: _root_.cats.kernel.Order[B]): _root_.cats.data.NonEmptySet[B] =
    src.asInstanceOf[_root_.cats.data.NonEmptySet[A]].map(f)(orderB)

  def traverseNonEmptySet[A, B](
      src: Any,
      f: A => partial.Result[B],
      failFast: Boolean,
      orderB: _root_.cats.kernel.Order[B]
  ): partial.Result[_root_.cats.data.NonEmptySet[B]] =
    partial.Result
      .traverse[Seq[B], A, B](src.asInstanceOf[_root_.cats.data.NonEmptySet[A]].toSortedSet.iterator, f, failFast)
      .map(seq => _root_.cats.data.NonEmptySet.of(seq.head, seq.tail*)(orderB))
}
