package io.scalaland.chimney

class CodecSpec extends ChimneySpec {

  // Domain <-> Dto: encode is total, decode validates (partial)
  private val intStringCodec: Codec[Int, String] = Codec(
    encode = (i: Int) => i.toString,
    decode = PartialTransformer[String, Int](str => partial.Result.fromCatching(str.toInt))
  )

  test("imapDomain") {
    case class Id(value: Int)

    val idCodec: Codec[Id, String] = intStringCodec.imapDomain(Id(_))(_.value)

    idCodec.encode.transform(Id(42)) ==> "42"
    idCodec.decode.transform("42") ==> partial.Result.fromValue(Id(42))
    idCodec.decode.transform("abc").asErrorPathMessageStrings ==> Iterable(("", """For input string: "abc""""))
  }

  test("imapDto") {
    case class Wrapper(raw: String)

    val wrappedCodec: Codec[Int, Wrapper] = intStringCodec.imapDto(Wrapper(_))(_.raw)

    wrappedCodec.encode.transform(42) ==> Wrapper("42")
    wrappedCodec.decode.transform(Wrapper("42")) ==> partial.Result.fromValue(42)
    wrappedCodec.decode.transform(Wrapper("abc")).asErrorPathMessageStrings ==> Iterable(
      ("", """For input string: "abc"""")
    )
  }
}
