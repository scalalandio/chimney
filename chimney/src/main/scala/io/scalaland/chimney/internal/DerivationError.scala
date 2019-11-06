package io.scalaland.chimney.internal

sealed trait DerivationError extends Product with Serializable {
  def sourceTypeName: String
  def targetTypeName: String
}

final case class MissingField(fieldName: String, fieldTypeName: String, sourceTypeName: String, targetTypeName: String)
    extends DerivationError

final case class MissingJavaBeanSetterParam(setterName: String,
                                            requiredTypeName: String,
                                            sourceTypeName: String,
                                            targetTypeName: String)
    extends DerivationError

final case class MissingTransformer(fieldName: String,
                                    sourceFieldTypeName: String,
                                    targetFieldTypeName: String,
                                    sourceTypeName: String,
                                    targetTypeName: String)
    extends DerivationError

final case class CantFindValueClassMember(sourceTypeName: String, targetTypeName: String) extends DerivationError

final case class CantFindCoproductInstanceTransformer(instance: String, sourceTypeName: String, targetTypeName: String)
    extends DerivationError

final case class IncompatibleSourceTuple(sourceArity: Int,
                                         targetArity: Int,
                                         sourceTypeName: String,
                                         targetTypeName: String)
    extends DerivationError

final case class NotSupportedDerivation(fieldName: String, sourceTypeName: String, targetTypeName: String)
    extends DerivationError

object DerivationError {

  def printErrors(errors: Seq[DerivationError]): String = {

    errors
      .groupBy(_.targetTypeName)
      .map {
        case (targetTypeName, errs) =>
          val errStrings = errs.distinct.map {
            case MissingField(fieldName, fieldTypeName, sourceTypeName, _) =>
              s"  $fieldName: $fieldTypeName - no accessor named $fieldName in source type $sourceTypeName"
            case MissingJavaBeanSetterParam(setterName, requiredTypeName, sourceTypeName, _) =>
              s"  set${setterName.capitalize}($setterName: $requiredTypeName) - no accessor named $setterName in source type $sourceTypeName"
            case MissingTransformer(fieldName, sourceFieldTypeName, targetFieldTypeName, sourceTypeName, _) =>
              s"  $fieldName: $targetFieldTypeName - can't derive transformation from $fieldName: $sourceFieldTypeName in source type $sourceTypeName"
            case CantFindValueClassMember(sourceTypeName, _) =>
              s"  can't find member of value class $sourceTypeName"
            case CantFindCoproductInstanceTransformer(instance, _, _) =>
              s"  can't transform coproduct instance $instance to $targetTypeName"
            case IncompatibleSourceTuple(sourceArity, targetArity, sourceTypeName, _) =>
              s"  source tuple $sourceTypeName is of arity $sourceArity, while target type $targetTypeName is of arity $targetArity; they need to be equal!"
            case NotSupportedDerivation(fieldName, sourceTypeName, _) =>
              s"  derivation from $fieldName: $sourceTypeName to $targetTypeName is not supported in Chimney!"
          }

          s"""$targetTypeName
           |${errStrings.mkString("\n")}
           |""".stripMargin
      }
      .mkString("\n")
  }
}
