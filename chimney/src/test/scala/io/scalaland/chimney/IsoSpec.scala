package io.scalaland.chimney

class IsoSpec extends ChimneySpec {

  case class Celsius(value: Int)
  case class Kelvin(value: Int)

  // First <-> Second, both directions total
  private val celsiusKelvin: Iso[Celsius, Kelvin] =
    Iso((c: Celsius) => Kelvin(c.value + 273), (k: Kelvin) => Celsius(k.value - 273))

  test("imapFirst") {
    // rebase the First side onto a raw Int
    val intKelvin: Iso[Int, Kelvin] = celsiusKelvin.imapFirst(_.value)(Celsius(_))

    intKelvin.first.transform(0) ==> Kelvin(273)
    intKelvin.second.transform(Kelvin(273)) ==> 0
  }

  test("imapSecond") {
    // rebase the Second side onto a raw Int
    val celsiusInt: Iso[Celsius, Int] = celsiusKelvin.imapSecond(_.value)(Kelvin(_))

    celsiusInt.first.transform(Celsius(0)) ==> 273
    celsiusInt.second.transform(273) ==> Celsius(0)
  }
}
