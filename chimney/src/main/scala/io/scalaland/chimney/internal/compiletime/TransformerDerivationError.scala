package io.scalaland.chimney.internal.compiletime

import Console.*
import io.scalaland.chimney.internal.compiletime.datatypes.ProductTypes.BeanAware

/** Transformer-specific error related to derivation logic */
sealed trait TransformerDerivationError extends Product with Serializable {
  def fromType: String
  def toType: String
}

final case class MissingConstructorArgument(
    toField: String,
    toFieldType: String,
    fromType: String,
    toType: String,
    accessorAvailable: Boolean = false
) extends TransformerDerivationError

final case class MissingJavaBeanSetterParam(
    toSetter: String,
    toSetterType: String,
    fromType: String,
    toType: String,
    accessorAvailable: Boolean = false
) extends TransformerDerivationError

final case class MissingFieldTransformer(
    toField: String,
    fromFieldType: String,
    toFieldType: String,
    fromType: String,
    toType: String
) extends TransformerDerivationError

final case class AmbiguousFieldSources(
    foundFromFields: List[String],
    toField: String, // TODO: comparator error
    fromType: String,
    toType: String
) extends TransformerDerivationError

final case class AmbiguousFieldOverrides(
    toName: String,
    foundOverrides: List[String],
    fieldNamesComparator: String,
    fromType: String,
    toType: String
) extends TransformerDerivationError

final case class MissingSubtypeTransformer(
    fromSubtype: String,
    fromType: String,
    toType: String
) extends TransformerDerivationError

final case class AmbiguousSubtypeTargets(
    fromSubtype: String,
    foundToSubtypes: List[String],
    fromType: String,
    toType: String
) extends TransformerDerivationError

final case class TupleArityMismatch(
    fromArity: Int,
    toArity: Int,
    fromType: String,
    toType: String
) extends TransformerDerivationError

final case class NotSupportedTransformerDerivation(
    exprPrettyPrint: String,
    fromType: String,
    toType: String
) extends TransformerDerivationError

object TransformerDerivationError {
  def printErrors(errors: Seq[TransformerDerivationError]): String =
    errors
      .groupBy(e => (e.toType, e.fromType))
      .map { case ((toType, fromType), errs) =>
        // TODO: add suggestions for inherited fields
        val errStrings = errs.distinct.map {
          case MissingConstructorArgument(toField, toFieldType, fromType, _, _) =>
            s"  $toField: $toFieldType - no accessor named $MAGENTA$toField$RESET in source type $fromType"
          case MissingJavaBeanSetterParam(toSetter, requiredTypeName, fromType, _, _) =>
            val toNormalized = BeanAware.dropSet(toSetter)
            s"  $toSetter($toNormalized: $requiredTypeName) - no accessor named $MAGENTA$toNormalized$RESET in source type $fromType"
          case MissingFieldTransformer(toField, fromFieldType, toFieldType, fromType, _) =>
            s"  $toField: $toFieldType - can't derive transformation from $toField: $fromFieldType in source type $fromType"
          case AmbiguousFieldSources(foundFromNames, toField, _, _) =>
            s"  field $toField: $toType has ambiguous matches in $fromType: ${foundFromNames.mkString(", ")}"
          case AmbiguousFieldOverrides(toName, foundOverrides, fieldNamesComparator, _, _) =>
            val overrides =
              foundOverrides.map(fieldOverride => s"$MAGENTA$fieldOverride$RESET").mkString(", ")
            s"  field $toName: $toType could not resolve overrides since the current $MAGENTA$fieldNamesComparator: TransformedNamedComparison$RESET treats the following overrides as the same: $overrides making it ambiguous - change the field name comparator with $MAGENTA.enableCustomFieldNameComparison$RESET to resolve the ambiguity"
          case MissingSubtypeTransformer(fromSubtype, _, _) =>
            s"  can't transform coproduct instance $fromSubtype to $toType"
          case AmbiguousSubtypeTargets(fromField, foundToFields, _, _) =>
            s"  coproduct instance $fromField of $fromType has ambiguous matches in $toType: ${foundToFields.mkString(", ")}"
          case TupleArityMismatch(fromArity, toArity, fromType, _) =>
            s"  source tuple $fromType is of arity $fromArity, while target type $toType is of arity $toArity; they need to be equal!"
          case NotSupportedTransformerDerivation(exprPrettyPrint, fromType, _) =>
            s"  derivation from $exprPrettyPrint: $fromType to $toType is not supported in Chimney!"
        }

        val fieldsWithMethodAccessor = errors.collect {
          case MissingConstructorArgument(toField, _, _, _, true)  => s"$MAGENTA$toField$RESET"
          case MissingJavaBeanSetterParam(toSetter, _, _, _, true) => s"$MAGENTA$toSetter$RESET"
        }.sorted
        val methodAccessorHint =
          if (fieldsWithMethodAccessor.nonEmpty) {
            val fields = fieldsWithMethodAccessor.take(3).mkString(", ") + (fieldsWithMethodAccessor.size match {
              case 1     => s" constructor argument/setter"
              case 2 | 3 => s" constructor arguments/setters"
              case _     => s" and ${fieldsWithMethodAccessor.length - 3} other constructor arguments/setters"
            })

            s"\nThere are methods in $fromType that might be used as accessors for $fields in $toType. Consider using $MAGENTA.enableMethodAccessors$RESET."
          } else ""

        s"""$toType
           |${errStrings.mkString("\n")}
           |$methodAccessorHint
           |""".stripMargin
      }
      .mkString("\n")
}
