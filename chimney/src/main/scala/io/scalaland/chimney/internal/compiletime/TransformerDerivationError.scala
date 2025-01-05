package io.scalaland.chimney.internal.compiletime

import Console.*
import io.scalaland.chimney.internal.compiletime.NotSupportedOperationFromPath.Operation as FromOperation
import io.scalaland.chimney.internal.compiletime.datatypes.ProductTypes.BeanAware

/** Transformer-specific error related to derivation logic */
sealed trait TransformerDerivationError extends Product with Serializable {
  def fromType: String
  def toType: String
}

final case class MissingConstructorArgument(
    toField: String,
    toFieldType: String,
    availableMethodAccessors: List[String],
    availableInheritedAccessors: List[String],
    availableDefault: Boolean,
    availableNone: Boolean
)(val fromType: String, val toType: String)
    extends TransformerDerivationError

final case class MissingJavaBeanSetterParam(
    toSetter: String,
    toSetterType: String,
    availableMethodAccessors: List[String],
    availableInheritedAccessors: List[String],
    availableNone: Boolean
)(val fromType: String, val toType: String)
    extends TransformerDerivationError

final case class MissingFieldTransformer(
    toField: String,
    fromFieldType: String,
    toFieldType: String
)(val fromType: String, val toType: String)
    extends TransformerDerivationError

final case class AmbiguousFieldSources(
    foundFromFields: List[String],
    toField: String
)(val fromType: String, val toType: String)
    extends TransformerDerivationError

final case class AmbiguousFieldOverrides(
    toName: String,
    foundOverrides: List[String],
    fieldNamesComparator: String
)(val fromType: String, val toType: String)
    extends TransformerDerivationError

final case class NotSupportedOperationFromPath(
    operation: NotSupportedOperationFromPath.Operation,
    toName: String,
    foundFromPath: String,
    allowedFromPaths: String
)(val fromType: String, val toType: String)
    extends TransformerDerivationError
object NotSupportedOperationFromPath {
  sealed trait Operation extends Product with Serializable
  object Operation {
    case object Computed extends Operation
    case object ComputedPartial extends Operation
    case object Renamed extends Operation
  }
}

final case class MissingSubtypeTransformer(
    fromSubtype: String
)(val fromType: String, val toType: String)
    extends TransformerDerivationError

final case class AmbiguousSubtypeTargets(
    fromSubtype: String,
    foundToSubtypes: List[String]
)(val fromType: String, val toType: String)
    extends TransformerDerivationError

final case class TupleArityMismatch(
    fromArity: Int,
    toArity: Int
)(val fromType: String, val toType: String)
    extends TransformerDerivationError

final case class AmbiguousImplicitPriority(
    totalExprPrettyPrint: String,
    partialExprPrettyPrint: String
)(val fromType: String, val toType: String)
    extends TransformerDerivationError

final case class NotSupportedTransformerDerivation(
    exprPrettyPrint: String
)(val fromType: String, val toType: String)
    extends TransformerDerivationError

object TransformerDerivationError {
  def printErrors(errors: Seq[TransformerDerivationError]): String =
    errors
      .groupBy(e => (e.toType, e.fromType))
      .map { case ((toType, fromType), errs) =>
        val errStrings = errs.distinct.map {
          case MissingConstructorArgument(toField, toFieldType, _, _, _, _) =>
            s"  $toField: $toFieldType - no accessor named $MAGENTA$toField$RESET in source type $fromType"
          case MissingJavaBeanSetterParam(toSetter, requiredTypeName, _, _, _) =>
            val toNormalized = BeanAware.dropSet(toSetter)
            s"  $toSetter($toNormalized: $requiredTypeName) - no accessor named $MAGENTA$toNormalized$RESET in source type $fromType"
          case MissingFieldTransformer(toField, fromFieldType, toFieldType) =>
            s"  $toField: $toFieldType - can't derive transformation from $toField: $fromFieldType in source type $fromType"
          case AmbiguousFieldSources(foundFromNames, toField) =>
            s"  field $toField: $toType has ambiguous matches in $fromType: ${foundFromNames.mkString(", ")}"
          case AmbiguousFieldOverrides(toName, foundOverrides, fieldNamesComparator) =>
            val overrides =
              foundOverrides.map(fieldOverride => s"$MAGENTA$fieldOverride$RESET").mkString(", ")
            s"  field $toName: $toType could not resolve overrides since the current $MAGENTA$fieldNamesComparator: TransformedNamedComparison$RESET treats the following overrides as the same: $overrides making it ambiguous - change the field name comparator with $MAGENTA.enableCustomFieldNameComparison$RESET to resolve the ambiguity"
          case NotSupportedOperationFromPath(operation, toName, foundFromPath, allowedFromPaths) =>
            operation match {
              case FromOperation.Computed =>
                s"  field $toName: computing from $foundFromPath is not allowed, try selecting only field names from $allowedFromPaths ($allowedFromPaths.field1.field2, etc)"
              case FromOperation.ComputedPartial =>
                s"  field $toName: partial computing from $foundFromPath is not allowed, try selecting only field names from $allowedFromPaths ($allowedFromPaths.field1.field2, etc)"
              case FromOperation.Renamed =>
                s"  field $toName: renaming from $foundFromPath is not allowed, try selecting only field names from $allowedFromPaths ($allowedFromPaths.field1.field2, etc)"
            }
          case MissingSubtypeTransformer(fromSubtype) =>
            s"  can't transform coproduct instance $fromSubtype to $toType"
          case AmbiguousSubtypeTargets(fromField, foundToFields) =>
            s"  coproduct instance $fromField of $fromType has ambiguous matches in $toType: ${foundToFields.mkString(", ")}"
          case TupleArityMismatch(fromArity, toArity) =>
            s"  source tuple $fromType is of arity $fromArity, while target type $toType is of arity $toArity; they need to be equal!"
          case AmbiguousImplicitPriority(totalExprPrettyPrint, partialExprPrettyPrint) =>
            s"""  ambiguous implicits while resolving Chimney recursive transformation!
               |    PartialTransformer[$fromType, $toType]: $partialExprPrettyPrint
               |    Transformer[$fromType, $toType]: $totalExprPrettyPrint
               |  Please eliminate total/partial ambiguity from implicit scope or use ${MAGENTA}enableImplicitConflictResolution$RESET/${MAGENTA}withFieldComputed$RESET/${MAGENTA}withFieldComputedPartial$RESET to decide which one should be used.""".stripMargin
          case NotSupportedTransformerDerivation(exprPrettyPrint) =>
            s"  derivation from $exprPrettyPrint: $fromType to $toType is not supported in Chimney!"
        }

        def prettyFieldList(fields: Seq[String])(use: String => String): String =
          if (fields.nonEmpty) {
            use(fields.take(3).mkString(", ") + (fields.size match {
              case 1     => s", the constructor argument/setter"
              case 2 | 3 => s", constructor arguments/setters"
              case _     => s" and ${fields.length - 3} other constructor arguments/setters"
            }))
          } else ""

        val methodAccessorHint = prettyFieldList(errors.collect {
          case MissingConstructorArgument(toField, _, availableMethodAccessors, _, _, _)
              if availableMethodAccessors.nonEmpty =>
            s"$MAGENTA$toField$RESET (e.g. ${availableMethodAccessors.map(a => s"$MAGENTA$a$RESET").mkString(", ")})"
          case MissingJavaBeanSetterParam(toSetter, _, availableMethodAccessors, _, _)
              if availableMethodAccessors.nonEmpty =>
            s"$MAGENTA$toSetter$RESET (e.g. ${availableMethodAccessors.map(a => s"$MAGENTA$a$RESET").mkString(", ")})"
        }.sorted) { fields =>
          s"\n\nThere are methods in $fromType that might be used as accessors for $fields in $toType. Consider using $MAGENTA.enableMethodAccessors$RESET."
        }

        val inheritedAccessorHint = prettyFieldList(errors.collect {
          case MissingConstructorArgument(toField, _, _, availableInheritedAccessors, _, _)
              if availableInheritedAccessors.nonEmpty =>
            s"$MAGENTA$toField$RESET (e.g. ${availableInheritedAccessors.map(a => s"$MAGENTA$a$RESET").mkString(", ")})"
          case MissingJavaBeanSetterParam(toSetter, _, _, availableInheritedAccessors, _)
              if availableInheritedAccessors.nonEmpty =>
            s"$MAGENTA$toSetter$RESET (e.g. ${availableInheritedAccessors.map(a => s"$MAGENTA$a$RESET").mkString(", ")})"
        }.sorted) { fields =>
          s"\n\nThere are inherited definitions in $fromType that might be used as accessors for $fields in $toType. Consider using $MAGENTA.enableInheritedAccessors$RESET."
        }

        val defaultValueHint = prettyFieldList(errors.collect {
          case MissingConstructorArgument(toField, _, _, _, true, _) => s"$MAGENTA$toField$RESET"
        }.sorted) { fields =>
          s"\n\nThere are default values for $fields in $toType. Consider using $MAGENTA.enableDefaultValues$RESET or $MAGENTA.enableDefaultValueForType$RESET."
        }

        val noneValueHint = prettyFieldList(errors.collect {
          case MissingConstructorArgument(toField, _, _, _, _, true) => s"$MAGENTA$toField$RESET"
          case MissingJavaBeanSetterParam(toSetter, _, _, _, true)   => s"$MAGENTA$toSetter$RESET"
        }.sorted) { fields =>
          s"\n\nThere are default optional values available for $fields in $toType. Consider using $MAGENTA.enableOptionDefaultsToNone$RESET."
        }

        s"""$toType
           |${errStrings.mkString("\n")}$methodAccessorHint$inheritedAccessorHint$defaultValueHint$noneValueHint
           |""".stripMargin
      }
      .mkString("\n")
}
