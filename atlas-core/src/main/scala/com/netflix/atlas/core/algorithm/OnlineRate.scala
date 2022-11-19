package com.netflix.atlas.core.algorithm

case class OnlineRate(buf: RollingBuffer) extends OnlineAlgorithm {

  import java.lang.{Double => JDouble}

  private[this] val n = buf.values.length
  private var currentSample = 0
  private var prevValue: Double = Double.NaN
  private var currentRate: Double = Double.NaN

  /** Apply the next value from the input and return the computed value.
    * This function expects one value to be fed for every step. */
  override def next(v: Double): Double = {
    currentSample += 1
    var currentValue = if(JDouble.isNaN(v) && !JDouble.isNaN(currentRate)) {
      // extrapolate if incoming value is NaN.
      prevValue + (currentRate * n)
    }
    else v

    if(!JDouble.isNaN(prevValue) && prevValue > currentValue) {
      currentValue += prevValue
    }

    buf.add(currentValue)
    val result = if(n == 1) currentValue
    else if (currentSample < n) Double.NaN
    else (buf.max - buf.min) / n
    currentRate = result
    prevValue = currentValue
    result
  }

  /** Reset the state of the algorithm. */
  override def reset(): Unit = {
    buf.clear()
    currentSample = 0
    prevValue = 0
    currentRate = 0
  }

  /**
    * Returns true if the state is the same as if it had been reset. This means that the state
    * does not need to be stored and can just be recreated if a new values shows up. When
    * processing a stream this is needed to avoid a memory leak for state objects if there are
    * some transient values associated with a group by. This check becomes the effective lifespan
    * for the state if no data is received for a given interval.
    */
  override def isEmpty: Boolean = buf.isEmpty

  /**
    * Capture the current state of the algorithm. It can be restored in a new instance
    * with the [OnlineAlgorithm#apply] method.
    */
  override def state: AlgoState = {
    AlgoState(
      "rate",
      "buffer" -> buf.state,
      "currentSample" -> currentSample,
      "currentRate" -> currentRate,
      "prevValue" -> prevValue
    )
  }
}

object OnlineRate {

  def apply(n: Int): OnlineRate = apply(RollingBuffer(n))

  def apply(state: AlgoState): OnlineRate = {
    val rate = apply(RollingBuffer(state.getState("buffer")))
    rate.currentSample = state.getInt("currentSample")
    rate.prevValue = state.getDouble("prevValue")
    rate.currentRate = state.getDouble("currentRate")
    rate
  }
}
