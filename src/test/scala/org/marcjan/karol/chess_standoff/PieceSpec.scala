package org.marcjan.karol.chess_standoff

import org.scalatest.{Matchers, FlatSpec}

class PieceSpec extends FlatSpec with Matchers {
  "A King" should "be able to move by one square in any direction" in {
    King.canMoveBy((1, 0)) should be (true)
    King.canMoveBy((-1, 0)) should be (true)

    King.canMoveBy((0, 1)) should be (true)
    King.canMoveBy((0, -1)) should be (true)

    King.canMoveBy((1, 1)) should be (true)
    King.canMoveBy((1, -1)) should be (true)
    King.canMoveBy((-1, 1)) should be (true)
    King.canMoveBy((1, -1)) should be (true)
  }

  it should "not be able to move beyond one square in any direction" in {
    King.canMoveBy((2, 2)) should be (false)
    King.canMoveBy((2, 1)) should be (false)
    King.canMoveBy((2, 0)) should be (false)
    King.canMoveBy((2, -1)) should be (false)
    King.canMoveBy((2, -2)) should be (false)

    King.canMoveBy((1, -2)) should be (false)
    King.canMoveBy((0, -2)) should be (false)
    King.canMoveBy((-1, -2)) should be (false)
    King.canMoveBy((-2, -2)) should be (false)

    King.canMoveBy((-2, -1)) should be (false)
    King.canMoveBy((-2, 0)) should be (false)
    King.canMoveBy((-2, 1)) should be (false)
    King.canMoveBy((-2, 2)) should be (false)

    King.canMoveBy((-1, 2)) should be (false)
    King.canMoveBy((0, 2)) should be (false)
    King.canMoveBy((1, 2)) should be (false)
  }
}
