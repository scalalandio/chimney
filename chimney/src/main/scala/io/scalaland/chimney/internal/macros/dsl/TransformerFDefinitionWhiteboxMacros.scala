package io.scalaland.chimney.internal.macros.dsl

import io.scalaland.chimney.internal.macros.TransformerConfigSupport
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.whitebox

class TransformerFDefinitionWhiteboxMacros(val c: whitebox.Context) extends MacroUtils with TransformerConfigSupport {

  import c.universe._
  import CfgTpes._

  def withFieldConstImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      T: WeakTypeTag,
      U: WeakTypeTag,
      C: WeakTypeTag
  ](selector: Tree, value: Tree): Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(weakTypeOf[U] <:< weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Value passed to `withFieldConst` is of type: ${weakTypeOf[U]}
           |Type required by '$fieldName' field: ${weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree
        .addOverride(fieldName, value)
        .refineConfig(fieldConstT.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }
  }

  def withFieldConstFImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      T: WeakTypeTag,
      U: WeakTypeTag,
      C: WeakTypeTag,
      F[_]
  ](selector: Tree, value: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(weakTypeOf[U] <:< weakTypeOf[T])) {
      val FT = F.tpe.applyTypeArg(weakTypeOf[T])
      val FU = F.tpe.applyTypeArg(weakTypeOf[U])
      val msg =
        s"""Type mismatch!
           |Value passed to `withFieldConstF` is of type: $FU
           |Type required by '$fieldName' field: $FT
           |${weakTypeOf[U]} is not subtype of ${weakTypeOf[T]}!
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree
        .addOverride(fieldName, value)
        .refineConfig(fieldConstFT.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }
  }

  def withFieldComputedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      T: WeakTypeTag,
      U: WeakTypeTag,
      C: WeakTypeTag
  ](selector: Tree, map: Tree): Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(weakTypeOf[U] <:< weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Function passed to `withFieldComputed` returns type: ${weakTypeOf[U]}
           |Type required by '$fieldName' field: ${weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree
        .addOverride(fieldName, map)
        .refineConfig(fieldComputedT.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }
  }

  def withFieldComputedFImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      T: WeakTypeTag,
      U: WeakTypeTag,
      C: WeakTypeTag,
      F[+_]
  ](selector: Tree, map: Tree)(implicit F: WeakTypeTag[F[_]]): Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(weakTypeOf[U] <:< weakTypeOf[T])) {
      val FT = F.tpe.applyTypeArg(weakTypeOf[T])
      val FU = F.tpe.applyTypeArg(weakTypeOf[U])

      val msg =
        s"""Type mismatch!
           |Function passed to `withFieldComputedF` returns type: $FU
           |Type required by '$fieldName' field: $FT
           |${weakTypeOf[U]} is not subtype of ${weakTypeOf[T]}!
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree
        .addOverride(fieldName, map)
        .refineConfig(fieldComputedFT.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }
  }

  def withFieldRenamedImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      T: WeakTypeTag,
      U: WeakTypeTag,
      C: WeakTypeTag
  ](selectorFrom: Tree, selectorTo: Tree): Tree = {

    val fieldNameFromOpt = selectorFrom.extractSelectorFieldNameOpt
    val fieldNameToOpt = selectorTo.extractSelectorFieldNameOpt

    (fieldNameFromOpt, fieldNameToOpt) match {
      case (Some(fieldNameFrom), Some(fieldNameTo)) =>
        c.prefix.tree
          .refineConfig(
            fieldRelabelledT.applyTypeArgs(fieldNameFrom.toSingletonTpe, fieldNameTo.toSingletonTpe, weakTypeOf[C])
          )
      case (Some(_), None) =>
        c.abort(c.enclosingPosition, s"Selector of type ${selectorTo.tpe} is not valid: $selectorTo")
      case (None, Some(_)) =>
        c.abort(c.enclosingPosition, s"Selector of type ${selectorFrom.tpe} is not valid: $selectorFrom")
      case (None, None) =>
        val inv1 = s"Selector of type ${selectorFrom.tpe} is not valid: $selectorFrom"
        val inv2 = s"Selector of type ${selectorTo.tpe} is not valid: $selectorTo"
        c.abort(c.enclosingPosition, s"Invalid selectors:\n$inv1\n$inv2")
    }
  }

  def withCoproductInstanceImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Inst: WeakTypeTag,
      C: WeakTypeTag
  ](f: Tree): Tree = {
    val To = weakTypeOf[To]
    val Inst = weakTypeOf[Inst]
    c.prefix.tree
      .addInstance(Inst.typeSymbol.fullName.toString, To.typeSymbol.fullName.toString, f)
      .refineConfig(coproductInstanceT.applyTypeArgs(Inst, To, weakTypeOf[C]))
  }

  def withCoproductInstanceFImpl[
      From: WeakTypeTag,
      To: WeakTypeTag,
      Inst: WeakTypeTag,
      C: WeakTypeTag
  ](f: Tree): Tree = {
    val To = weakTypeOf[To]
    val Inst = weakTypeOf[Inst]
    c.prefix.tree
      .addInstance(Inst.typeSymbol.fullName.toString, To.typeSymbol.fullName.toString, f)
      .refineConfig(coproductInstanceFT.applyTypeArgs(Inst, To, weakTypeOf[C]))
  }

}
