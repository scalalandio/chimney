package io.scalaland.chimney.internal.macros

import io.scalaland.chimney.internal.utils.{DslMacroUtils, TypeTestUtils}

import scala.reflect.macros.blackbox

trait GenTrees extends Model with TypeTestUtils with DslMacroUtils {

  val c: blackbox.Context

  import c.universe.*

  object Trees {
    val any: Tree = tq"_root_.scala.Any"
    val arrayAny: Tree = arrayTpe(any)
    val unit: Tree = q"()"

    val intTpe: Tree = tq"_root_.scala.Int"

    def array(args: Seq[Tree]): Tree = {
      q"_root_.scala.Array(..${args})"
    }

    def arrayTpe(inTpe: Tree): Tree = {
      tq"_root_.scala.Array[$inTpe]"
    }

    object Option {
      val none: Tree = q"_root_.scala.None"
      def tpe(t: Type): Tree = {
        tq"_root_.scala.Option[$t]"
      }
      def option(t: Type, value: Tree): Tree = {
        q"_root_.scala.Option[$t]($value)"
      }
      def empty(t: Type): Tree = {
        q"_root_.scala.Option.empty[$t]"
      }
      def apply(t: Type): Tree = {
        q"_root_.scala.Option.apply[$t]"
      }
    }

    object Either {
      def left(value: Tree): Tree = {
        q"new _root_.scala.util.Left($value)"
      }
      def right(value: Tree): Tree = {
        q"new _root_.scala.util.Right($value)"
      }
    }

    object Transformer {
      def tpe(From: Type, To: Type): Tree = {
        tq"_root_.io.scalaland.chimney.Transformer[$From, $To]"
      }
    }

    object PartialTransformer {
      def tpe(From: Type, To: Type): Tree = {
        tq"_root_.io.scalaland.chimney.PartialTransformer[$From, $To]"
      }
    }

    object LiftedTransformer {
      def tpe(F: Type, From: Type, To: Type): Tree = {
        tq"_root_.io.scalaland.chimney.TransformerF[$F, $From, $To]"
      }
    }

    object Patcher {
      def tpe(T: Type, Patch: Type): Tree = {
        tq"_root_.io.scalaland.chimney.Patcher[$T, $Patch]"
      }
    }

    object PartialResult {

      def empty: Tree = {
        q"_root_.io.scalaland.chimney.partial.Result.fromEmpty"
      }

      def value(valTree: Tree): Tree = {
        q"_root_.io.scalaland.chimney.partial.Result.Value($valTree)"
      }

      def patValue(termName: TermName): Tree = {
        pq"_root_.io.scalaland.chimney.partial.Result.Value($termName)"
      }

      def fromFunction(f: Tree): Tree = {
        q"_root_.io.scalaland.chimney.partial.Result.fromFunction($f)"
      }

      def map2(aTpe: Type, bTpe: Type, cTpe: Type, res1: Tree, res2: Tree, f: Tree, failFast: Tree): Tree = {
        q"_root_.io.scalaland.chimney.partial.Result.map2[$aTpe, $bTpe, $cTpe]($res1, $res2, $f, $failFast)"
      }

      def product(aTpe: Type, bTpe: Type, res1: Tree, res2: Tree, failFast: Tree): Tree = {
        q"_root_.io.scalaland.chimney.partial.Result.product[$aTpe, $bTpe]($res1, $res2, $failFast)"
      }

      def sequence(mTpe: Tree, aTpe: Tree, it: Tree, failFast: Tree): Tree = {
        q"_root_.io.scalaland.chimney.partial.Result.sequence[$mTpe, $aTpe]($it, $failFast)"
      }

      def traverse(mTpe: Tree, aTpe: Tree, bTpe: Tree, it: Tree, f: Tree, failFast: Tree): Tree = {
        q"_root_.io.scalaland.chimney.partial.Result.traverse[$mTpe, $aTpe, $bTpe]($it, $f, $failFast)"
      }
    }

    object PartialErrors {
      val tpe: Tree = tq"_root_.io.scalaland.chimney.partial.Result.Errors"

      def merge(tn1: TermName, tn2: TermName): Tree = {
        q"_root_.io.scalaland.chimney.partial.Result.Errors.merge($tn1, $tn2)"
      }
    }

    object PathElement {
      def accessor(targetName: String): Tree = {
        q"_root_.io.scalaland.chimney.partial.PathElement.Accessor($targetName)"
      }

      def index(idxIdent: Ident): Tree = {
        q"_root_.io.scalaland.chimney.partial.PathElement.Index($idxIdent)"
      }

      def mapKey(keyIdent: Ident): Tree = {
        q"_root_.io.scalaland.chimney.partial.PathElement.MapKey($keyIdent)"
      }

      def mapValue(keyIdent: Ident): Tree = {
        q"_root_.io.scalaland.chimney.partial.PathElement.MapValue($keyIdent)"
      }
    }
  }

}
