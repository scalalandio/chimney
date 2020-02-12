package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.MacroUtils

import scala.reflect.macros.whitebox

class TransformerDefinitionWhiteboxMacros(val c: whitebox.Context) extends DslWhiteboxMacros with MacroUtils {
  import c.universe._

  def withFieldConstImpl[
      F[_]: WTTF,
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](
      selector: c.Tree,
      value: c.Tree
  ): c.Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(c.weakTypeOf[U] <:< c.weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Value passed to `withFieldConst` is of type: ${c.weakTypeOf[U]}
           |Type required by '$fieldName' field: ${c.weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      expandWithFieldConstF[F, From, To, C](
        fieldName,
        q"_root_.io.scalaland.chimney.TransformationContext[${WTTF[F]}].pure[${weakTypeTag[U]}]($value)"
      )
    }
  }

  def withFieldConstImplF[
      F[_]: WTTF,
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      T: c.WeakTypeTag,
      U: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](
      selector: c.Tree,
      value: c.Tree
  ): c.Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(c.weakTypeOf[U] <:< c.weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Value passed to `withFieldConstF` is of type: ${WTTF[F]}[${c.weakTypeOf[U]}]
           |Type required by '$fieldName' field: ${WTTF[F]}[${c.weakTypeOf[T]}]
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      expandWithFieldConstF[F, From, To, C](fieldName, value)
    }
  }

  def withFieldComputedImpl[
      F[_]: WTTF,
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
      expandFieldComputedF[F, From, To, C](
        fieldName,
        q"$map.andThen(_root_.io.scalaland.chimney.TransformationContext[${WTTF[F]}].pure[${weakTypeTag[U]}])"
      )
    }
  }

  def withFieldComputedImplF[
      F[_]: WTTF,
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
           |Function passed to `withFieldComputedF` returns type: ${WTTF[F]}[${c.weakTypeOf[U]}]
           |Type required by '$fieldName' field: ${WTTF[F]}[${c.weakTypeOf[T]}]
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      expandFieldComputedF[F, From, To, C](fieldName, map)
    }
  }

  def withFieldRenamedImpl[
      F[_]: WTTF,
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
        expandFieldRenamed[F, From, To, C](fieldNameFrom, fieldNameTo)

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
      F[_]: WTTF,
      From: c.WeakTypeTag,
      To: c.WeakTypeTag,
      Inst: c.WeakTypeTag,
      C: c.WeakTypeTag
  ](
      f: c.Tree
  ): c.Tree = {
    expandCoproductInstance[F, From, To, Inst, C](f)
  }

}
