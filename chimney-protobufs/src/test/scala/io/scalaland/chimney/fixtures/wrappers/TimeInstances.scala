package io.scalaland.chimney.fixtures.wrappers

import java.time.Instant
import scala.concurrent.duration.{Duration, FiniteDuration}

final case class TimeInstances(duration1: FiniteDuration, duration2: Duration, timestamp: Instant)
