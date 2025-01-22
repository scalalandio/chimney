package io.scalaland.chimney.dsl

/** What should happen when derivation is dropping some target subtype without using it for anything.
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#checking-for-unused-source-fieldsunmatched-target-subtypes]] for more
  *   details
  *
  * @since 1.7.0
  */
sealed abstract class UnmatchedSubtypePolicy

/** Tell the derivation that silent drop of a target subtype is an error (explicit drop is not).
  *
  * @see
  *   [[https://chimney.readthedocs.io/cookbook/#checking-for-unused-source-fieldsunmatched-target-subtypes]] for more
  *   details
  *
  * @since 1.7.0
  */
case object FailOnUnmatchedTargetSubtype extends UnmatchedSubtypePolicy
