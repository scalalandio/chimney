package io.scalaland.chimney.internal.utils

import scala.reflect.macros.blackbox

trait CompanionUtils {

  val c: blackbox.Context

  import c.internal._
  import c.universe._

  // Borrowed from jsoniter-scala: https://github.com/plokhotnyuk/jsoniter-scala/blob/b14dbe51d3ae6752e5a9f90f1f3caf5bceb5e4b0/jsoniter-scala-macros/shared/src/main/scala/com/github/plokhotnyuk/jsoniter_scala/macros/JsonCodecMaker.scala#L462
  def companionSymbol(tpe: Type): Symbol = {
    val comp = tpe.typeSymbol.companion
    if (comp.isModule) comp
    else {
      val ownerChainOf: Symbol => Iterator[Symbol] =
        s => Iterator.iterate(s)(_.owner).takeWhile(x => x != null && x != NoSymbol).toVector.reverseIterator
      val path = ownerChainOf(tpe.typeSymbol)
        .zipAll(ownerChainOf(enclosingOwner), NoSymbol, NoSymbol)
        .dropWhile { case (x, y) => x == y }
        .takeWhile(_._1 != NoSymbol)
        .map(_._1.name.toTermName)
      if (path.isEmpty) c.abort(c.enclosingPosition, s"Cannot find a companion for $tpe")
      else c.typecheck(path.foldLeft[Tree](Ident(path.next()))(Select(_, _)), silent = true).symbol
    }
  }
}
