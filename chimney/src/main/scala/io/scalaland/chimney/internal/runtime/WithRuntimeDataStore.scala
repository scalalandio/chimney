package io.scalaland.chimney.internal.runtime

trait WithRuntimeDataStore {

  private[chimney] def addOverride(overrideData: Any): this.type
}
object WithRuntimeDataStore {

  def update[A <: WithRuntimeDataStore](withRuntimeDataStore: A, overrideData: Any): A =
    withRuntimeDataStore.addOverride(overrideData)
}
