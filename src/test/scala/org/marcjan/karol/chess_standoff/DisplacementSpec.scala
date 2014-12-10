package org.marcjan.karol.chess_standoff

import org.scalatest.{Matchers, FlatSpec}

class DisplacementSpec extends FlatSpec with Matchers {

  import Displacement._

  "isDiagonal" should "hold for (1, 1)" in {
    isDiagonal((1, 1)) should be (true)
  }

   it should "hold for (2, 2)" in {
    isDiagonal((2, 2)) should be (true)
  }

  it should "hold for (-1, -1)" in {
    isDiagonal((-1, -1)) should be (true)
  }

  it should "not hold for (2, 3)" in {
    isDiagonal((2, 3)) should be (false)
  }

  it should "not hold for (-3, 2)" in {
    isDiagonal((-3, 2)) should be (false)
  }
}
