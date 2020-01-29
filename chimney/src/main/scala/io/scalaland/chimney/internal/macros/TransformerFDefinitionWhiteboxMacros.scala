package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.TransformerConfiguration
import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.whitebox

class TransformerFDefinitionWhiteboxMacros(val c: whitebox.Context) extends MacroUtils with TransformerConfiguration {

  import c.universe._
  import CfgTpeConstructors._

  def withFieldConstImpl[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](selector: c.Tree, value: c.Tree): c.Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(c.weakTypeOf[U] <:< c.weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Value passed to `withFieldConst` is of type: ${c.weakTypeOf[U]}
           |Type required by '$fieldName' field: ${c.weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree
        .addOverride(fieldName, value)
        .refineConfig(fieldConstT.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }
  }

  def withFieldConstFImpl[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag,
      F[_]
  ](selector: c.Tree, value: c.Tree)(implicit F: c.WeakTypeTag[F[_]]): c.Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(c.weakTypeOf[U] <:< c.weakTypeOf[T])) {
      val FT = F.tpe.applyTypeArg(c.weakTypeOf[T])
      val FU = F.tpe.applyTypeArg(c.weakTypeOf[U])
      val msg =
        s"""Type mismatch!
           |Value passed to `withFieldConstF` is of type: $FU
           |Type required by '$fieldName' field: $FT
           |${c.weakTypeOf[U]} is not subtype of ${c.weakTypeOf[T]}!
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree
        .addOverride(fieldName, value)
        .refineConfig(fieldConstFT.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }
  }

  def withFieldComputedImpl[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](selector: c.Tree, map: c.Tree): c.Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(c.weakTypeOf[U] <:< c.weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Function passed to `withFieldComputed` returns type: ${c.weakTypeOf[U]}
           |Type required by '$fieldName' field: ${c.weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree
        .addOverride(fieldName, map)
        .refineConfig(fieldComputedT.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }
  }

  def withFieldComputedFImpl[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag,
      F[+_]
  ](selector: c.Tree, map: c.Tree)(implicit F: c.WeakTypeTag[F[_]]): c.Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(c.weakTypeOf[U] <:< c.weakTypeOf[T])) {
      val FT = F.tpe.applyTypeArg(c.weakTypeOf[T])
      val FU = F.tpe.applyTypeArg(c.weakTypeOf[U])

      val msg =
        s"""Type mismatch!
           |Function passed to `withFieldComputedF` returns type: $FU
           |Type required by '$fieldName' field: $FT
           |${c.weakTypeOf[U]} is not subtype of ${c.weakTypeOf[T]}!
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      c.prefix.tree
        .addOverride(fieldName, map)
        .refineConfig(fieldComputedFT.applyTypeArgs(fieldName.toSingletonTpe, weakTypeOf[C]))
    }
  }

  def withFieldRenamedImpl[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](selectorFrom: c.Tree, selectorTo: c.Tree): c.Tree = {

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
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Inst: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](f: c.Tree): c.Tree = {
    val To = weakTypeOf[To]
    val Inst = weakTypeOf[Inst]
    c.prefix.tree
      .addInstance(Inst.typeSymbol.fullName.toString, To.typeSymbol.fullName.toString, f)
      .refineConfig(coproductInstanceT.applyTypeArgs(Inst, To, weakTypeOf[C]))
  }

  def withCoproductInstanceFImpl[
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Inst: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](f: c.Tree): c.Tree = {
    val To = weakTypeOf[To]
    val Inst = weakTypeOf[Inst]
    c.prefix.tree
      .addInstance(Inst.typeSymbol.fullName.toString, To.typeSymbol.fullName.toString, f)
      .refineConfig(coproductInstanceFT.applyTypeArgs(Inst, To, weakTypeOf[C]))
  }

}
