package io.scalaland.chimney.benchmarks

import org.openjdk.jmh.annotations.{Benchmark, Level, Setup}
import io.scalaland.chimney.dsl.*
import io.scalaland.chimney.Transformer

class Coproduct extends CommonBenchmarkSettings {

  import fixtures.*

  private val color2ChannelT = Transformer.derive[Color, Channel]
  private val channel2ColorT = Transformer
    .define[Channel, Color]
    .withSealedSubtypeHandled { (_: Channel.Alpha.type) =>
      Color.Blue
    }
    .buildTransformer

  var color: Color = Color.Red

  @Setup(Level.Iteration)
  def nextColor(): Unit =
    color = color match {
      case Color.Red   => Color.Green
      case Color.Green => Color.Blue
      case Color.Blue  => Color.Red
    }

  @Benchmark
  def coproductIsomorphismChimneyInto: Color =
    color
      .transformInto[Channel]
      .into[Color]
      .withSealedSubtypeHandled { (_: Channel.Alpha.type) =>
        Color.Blue
      }
      .transform

  @Benchmark
  def coproductIsomorphismChimneyDefined: Color = channel2ColorT.transform(color2ChannelT.transform(color))

  @Benchmark
  def coproductIsomorphismByHand: Color = channel2Color(color2Channel(color))

}
