package com.netflix.atlas.core.algorithm


class OnlineRateSuite extends BaseOnlineAlgorithmSuite {

  test("n = 1") {
    val algo = OnlineRate(1)
    assertEquals(algo.next(0.0), 0.0)
    assertEquals(algo.next(1.0), 1.0)
  }

  test("n = 2") {
    val algo = OnlineRate(2)
    assert(algo.next(0.0).isNaN)
    assertEquals(algo.next(1.0), 0.5)
    assertEquals(algo.next(2.0), 0.5)
  }

  test("n = 2, decreasing because of client reset") {
    val algo = OnlineRate(2)
    assert(algo.next(2.0).isNaN)
    assertEquals(algo.next(1.5), (3.5 - 2.0) / 2)
    assertEquals(algo.next(1.6), (5.1 - 3.5) / 2)
  }


  test("n = 2, NaNs") {
    val algo = OnlineRate(2)
    assert(algo.next(0.0).isNaN)
    assertEquals(algo.next(1.0), 0.5)
    assertEquals(algo.next(2.0), 0.5)
    assertEquals(algo.next(Double.NaN), 0.5)
    assert(!algo.isEmpty)
  }

  override protected def newInstance: OnlineAlgorithm = OnlineRate(60)
}
