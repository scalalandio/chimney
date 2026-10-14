package io.scalaland.chimney

import io.scalaland.chimney.dsl.*

/** This module's tests are compiled with `-Xmacro-settings:chimney.nowarn=true` (and explicit
  * `chimney.SuppressWarnings`), so the generated code is annotated with both `@SuppressWarnings` and `@nowarn`.
  *
  * The regression check is that this spec COMPILES: a position-less `@nowarn` used to crash Scala 2 with
  * `UnsupportedOperationException: Position.point on NoPosition` (https://github.com/scalalandio/chimney/issues/966);
  * the derived values are asserted for good measure.
  */
class MacroSettingsSpec extends ChimneySpec {
  import MacroSettingsSpec.*

  test("total Transformer derived with chimney.nowarn") {
    val transformer: Transformer[AccountDto, Account] =
      Transformer
        .define[AccountDto, Account]
        .withFieldRenamed(_.Id, _.id)
        .withFieldComputed(_.flag, _.Flag.getOrElse(false))
        .buildTransformer

    transformer.transform(AccountDto("id", Some(true))) ==> Account("id", flag = true)
    AccountDto("id", None).into[Account].withFieldRenamed(_.Id, _.id).withFieldConst(_.flag, true).transform ==>
      Account("id", flag = true)
  }

  test("PartialTransformer derived with chimney.nowarn") {
    val transformer: PartialTransformer[AccountDto, Account] =
      PartialTransformer
        .define[AccountDto, Account]
        .withFieldRenamed(_.Id, _.id)
        .withFieldComputedPartial(_.flag, dto => partial.Result.fromOption(dto.Flag))
        .buildTransformer

    transformer.transform(AccountDto("id", Some(true))).asOption ==> Some(Account("id", flag = true))
    transformer.transform(AccountDto("id", None)).asOption ==> None
  }

  test("Patcher derived with chimney.nowarn") {
    Account("id", flag = false).patchUsing(AccountPatch(flag = true)) ==> Account("id", flag = true)
  }
}
object MacroSettingsSpec {
  final case class AccountDto(Id: String, Flag: Option[Boolean])
  final case class Account(id: String, flag: Boolean)
  final case class AccountPatch(flag: Boolean)
}
