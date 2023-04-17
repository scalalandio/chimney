package io.scalaland.chimney.internal

// TODO: move to compiletime once all rules are migrated and old macros removed
sealed trait TransformerDerivationError extends Product with Serializable {
  def sourceTypeName: String
  def targetTypeName: String
}

final case class MissingAccessor(
    fieldName: String,
    fieldTypeName: String,
    sourceTypeName: String,
    targetTypeName: String,
    defAvailable: Boolean = false
) extends TransformerDerivationError

final case class MissingJavaBeanSetterParam(
    setterName: String,
    requiredTypeName: String,
    sourceTypeName: String,
    targetTypeName: String
) extends TransformerDerivationError

final case class MissingTransformer(
    fieldName: String,
    sourceFieldTypeName: String,
    targetFieldTypeName: String,
    sourceTypeName: String,
    targetTypeName: String
) extends TransformerDerivationError

final case class CantFindValueClassMember(sourceTypeName: String, targetTypeName: String)
    extends TransformerDerivationError

final case class CantFindCoproductInstanceTransformer(instance: String, sourceTypeName: String, targetTypeName: String)
    extends TransformerDerivationError

final case class AmbiguousCoproductInstance(instance: String, sourceTypeName: String, targetTypeName: String)
    extends TransformerDerivationError

final case class IncompatibleSourceTuple(
    sourceArity: Int,
    targetArity: Int,
    sourceTypeName: String,
    targetTypeName: String
) extends TransformerDerivationError

final case class NotSupportedTransformerDerivation(fieldName: String, sourceTypeName: String, targetTypeName: String)
    extends TransformerDerivationError

final case class MacroException(exception: Throwable, sourceTypeName: String, targetTypeName: String)
    extends TransformerDerivationError

final case class NotYetImplemented(what: String, sourceTypeName: String, targetTypeName: String)
    extends TransformerDerivationError

object TransformerDerivationError {
  def printErrors(errors: Seq[TransformerDerivationError]): String = {

    errors
      .groupBy(e => (e.targetTypeName, e.sourceTypeName))
      .map { case ((targetTypeName, sourceTypeName), errs) =>
        val errStrings = errs.distinct.map {
          case MissingAccessor(fieldName, fieldTypeName, sourceTypeName, _, _) =>
            s"  $fieldName: $fieldTypeName - no accessor named $fieldName in source type $sourceTypeName"
          case MissingJavaBeanSetterParam(setterName, requiredTypeName, sourceTypeName, _) =>
            s"  set${setterName.capitalize}($setterName: $requiredTypeName) - no accessor named $setterName in source type $sourceTypeName"
          case MissingTransformer(fieldName, sourceFieldTypeName, targetFieldTypeName, sourceTypeName, _) =>
            s"  $fieldName: $targetFieldTypeName - can't derive transformation from $fieldName: $sourceFieldTypeName in source type $sourceTypeName"
          case CantFindValueClassMember(sourceTypeName, _) =>
            s"  can't find member of value class $sourceTypeName"
          case CantFindCoproductInstanceTransformer(instance, _, _) =>
            s"  can't transform coproduct instance $instance to $targetTypeName"
          case AmbiguousCoproductInstance(instance, _, _) =>
            s"  coproduct instance $instance of $targetTypeName is ambiguous"
          case IncompatibleSourceTuple(sourceArity, targetArity, sourceTypeName, _) =>
            s"  source tuple $sourceTypeName is of arity $sourceArity, while target type $targetTypeName is of arity $targetArity; they need to be equal!"
          case NotSupportedTransformerDerivation(fieldName, sourceTypeName, _) =>
            s"  derivation from $fieldName: $sourceTypeName to $targetTypeName is not supported in Chimney!"
          case MacroException(exception, _, _) =>
            val stackTrace = exception.getStackTrace.view.map(ste => s"  \t$ste").mkString("\n")
            s"  macro expansion thrown exception!: ${exception.getMessage}:\n$stackTrace"
          case NotYetImplemented(what, _, _) =>
            s"  derivation failed because functionality $what is not yet implemented!"
        }

        val fieldsWithMethodAccessor = errors.collect { case MissingAccessor(fieldName, _, _, _, true) =>
          s"`$fieldName`"
        }
        val methodAccessorHint =
          if (fieldsWithMethodAccessor.nonEmpty) {
            val first3Fields = fieldsWithMethodAccessor.take(3).mkString(", ")
            val otherFields = fieldsWithMethodAccessor.length - 3
            val fields = if (otherFields > 0) s"$first3Fields and $otherFields other methods" else first3Fields

            s"\nThere are methods in $sourceTypeName that might be used as accessors for $fields fields in $targetTypeName. Consider using `.enableMethodAccessors`."
          } else ""

        s"""$targetTypeName
           |${errStrings.mkString("\n")}
           |$methodAccessorHint
           |""".stripMargin
      }
      .mkString("\n")
  }
}
