package org.marcjan.karol.chess_standoff

import org.scalatest.{Matchers, FlatSpec}

class DisplacementSpec extends FlatSpec with Matchers {

  import Displacement._

  "(1, 1)" should "be diagonal" in {
    isDiagonal((1, 1)) should be (true)
  }

   "(2, 2)" should "be diagonal" in {
    isDiagonal((2, 2)) should be (true)
  }
}
