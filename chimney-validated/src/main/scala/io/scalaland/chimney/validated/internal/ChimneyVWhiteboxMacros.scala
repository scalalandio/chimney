package io.scalaland.chimney.validated.internal

import io.scalaland.chimney.internal.MacroUtils

import scala.reflect.macros.whitebox

class ChimneyVWhiteboxMacros(val c: whitebox.Context) extends DslVWhiteboxMacros with MacroUtils {
  def withFieldConstVImpl[From: c.WeakTypeTag,
                          To: c.WeakTypeTag,
                          T: c.WeakTypeTag,
                          U: c.WeakTypeTag,
                          C: c.WeakTypeTag,
                          VC: c.WeakTypeTag](selector: c.Tree, value: c.Tree): c.Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(c.weakTypeOf[U] <:< c.weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Value passed to `withFieldConstV` is of type: IV[${c.weakTypeOf[U]}]
           |Type required by '$fieldName' field: IV[${c.weakTypeOf[T]}]
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      expandWithFieldConstV[From, To, C, VC](fieldName, value)
    }
  }

  def withFieldComputedVImpl[From: c.WeakTypeTag,
                             To: c.WeakTypeTag,
                             T: c.WeakTypeTag,
                             U: c.WeakTypeTag,
                             C: c.WeakTypeTag,
                             VC: c.WeakTypeTag](selector: c.Tree, map: c.Tree): c.Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(c.weakTypeOf[U] <:< c.weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Function passed to `withFieldComputed` returns type: IV[${c.weakTypeOf[U]}]
           |Type required by '$fieldName' field: IV[${c.weakTypeOf[T]}]
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      expandFieldComputedV[From, To, C, VC](fieldName, map)
    }
  }

  def withFieldConstImpl[From: c.WeakTypeTag,
                         To: c.WeakTypeTag,
                         T: c.WeakTypeTag,
                         U: c.WeakTypeTag,
                         C: c.WeakTypeTag,
                         VC: c.WeakTypeTag](selector: c.Tree, value: c.Tree): c.Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(c.weakTypeOf[U] <:< c.weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Value passed to `withFieldConst` is of type: ${c.weakTypeOf[U]}
           |Type required by '$fieldName' field: ${c.weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      expandWithFieldConst[From, To, C, VC](fieldName, value)
    }
  }

  def withFieldComputedImpl[From: c.WeakTypeTag,
                            To: c.WeakTypeTag,
                            T: c.WeakTypeTag,
                            U: c.WeakTypeTag,
                            C: c.WeakTypeTag,
                            VC: c.WeakTypeTag](selector: c.Tree, map: c.Tree): c.Tree = {
    val fieldName = selector.extractSelectorFieldName

    if (!(c.weakTypeOf[U] <:< c.weakTypeOf[T])) {
      val msg =
        s"""Type mismatch!
           |Function passed to `withFieldComputed` returns type: ${c.weakTypeOf[U]}
           |Type required by '$fieldName' field: ${c.weakTypeOf[T]}
         """.stripMargin

      c.abort(c.enclosingPosition, msg)
    } else {
      expandFieldComputed[From, To, C, VC](fieldName, map)
    }
  }

  def withFieldRenamedImpl[From: c.WeakTypeTag,
                           To: c.WeakTypeTag,
                           T: c.WeakTypeTag,
                           U: c.WeakTypeTag,
                           C: c.WeakTypeTag,
                           VC: c.WeakTypeTag](selectorFrom: c.Tree, selectorTo: c.Tree): c.Tree = {

    val fieldNameFromOpt = selectorFrom.extractSelectorFieldNameOpt
    val fieldNameToOpt = selectorTo.extractSelectorFieldNameOpt

    (fieldNameFromOpt, fieldNameToOpt) match {
      case (Some(fieldNameFrom), Some(fieldNameTo)) =>
        expandFieldRenamed[From, To, C, VC](fieldNameFrom, fieldNameTo)

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
}
