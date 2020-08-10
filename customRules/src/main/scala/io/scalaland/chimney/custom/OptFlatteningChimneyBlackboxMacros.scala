package io.scalaland.chimney.custom

import io.scalaland.chimney.internal.macros.ChimneyBlackboxMacros

import scala.reflect.macros.blackbox

class OptFlatteningChimneyBlackboxMacros(override val c: blackbox.Context) extends ChimneyBlackboxMacros(c) {
  import c.universe._

  def optIterableOrArray(t: Type): Boolean = {
    isOption(t) && (t <:< noneTpe || iterableOrArray(t.typeArgs.head))
  }

  override def defaultConfig: TransformerConfig = {
    val optFlattening = TransformationRule(
      fromCondition = optIterableOrArray,
      toCondition = iterableOrArray,
      derivation = expandOptCollectionFlattening
    )

    TransformerConfig(customRules = List(optFlattening))
  }

  def expandOptCollectionFlattening(srcPrefixTree: Tree, config: TransformerConfig)(
      From: Type,
      To: Type
  ): TransformationResult = {
    val ToInnerT = To.collectionInnerTpe

    val emptyTree = mkTransformerBodyTree0(config)(
      q"_root_.scala.collection.immutable.List.empty[$ToInnerT]".convertCollection(To, ToInnerT)
    )

    if (From <:< noneTpe) {
      Right(emptyTree)
    } else {
      val FromCollectionT = From.typeArgs.head
      val fn = Ident(freshTermName(srcPrefixTree))

      resolveRecursiveTransformerBody(fn, config.rec)(FromCollectionT, To).mapRight { nonEmptyT =>
        val nonEmptyTree =
          if (nonEmptyT.isWrapped)
            nonEmptyT.tree
          else mkTransformerBodyTree0(config)(nonEmptyT.tree)

        val outTpe = config.wrapperType.fold(To)(_.applyTypeArg(To))

        q"$srcPrefixTree.fold[$outTpe]($emptyTree)(($fn: $FromCollectionT) => $nonEmptyTree)"
      }
    }
  }
}
