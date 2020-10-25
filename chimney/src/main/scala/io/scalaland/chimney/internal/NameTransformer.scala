package io.scalaland.chimney.internal

import java.util.regex.Pattern

case class NameTransformer(transformNameInto: String => String, transformNameFrom: String => String)

object NameTransformer {
  def apply(snakeToCamel: Boolean, camelToSnake: Boolean): NameTransformer = {
    NameTransformer(useSnakeCaseTransformation(camelToSnake), useSnakeCaseTransformation(snakeToCamel))
  }

  private def useSnakeCaseTransformation(value: Boolean): String => String = {
    if (value) {
      snakeCaseTransformation
    } else {
      identity
    }
  }

  // This code is copied from circe
  private val basePattern: Pattern = Pattern.compile("([A-Z]+)([A-Z][a-z])")
  private val swapPattern: Pattern = Pattern.compile("([a-z\\d])([A-Z])")

  private val snakeCaseTransformation: String => String = s => {
    val partial = basePattern.matcher(s).replaceAll("$1_$2")
    swapPattern.matcher(partial).replaceAll("$1_$2").toLowerCase
  }
}
