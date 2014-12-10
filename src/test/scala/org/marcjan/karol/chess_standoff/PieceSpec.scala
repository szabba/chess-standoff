package org.marcjan.karol.chess_standoff

import org.scalatest.{Matchers, FlatSpec}

class PieceSpec extends FlatSpec with Matchers {
  "A King" should "be able to move by one square in any direction" in {
    King.canMoveBy((1, 0)) should be (true)
    King.canMoveBy((-1, 0)) should be (true)
    King.canMoveBy((0, 1)) should be (true)
    King.canMoveBy((0, -1)) should be (true)
  }
}
