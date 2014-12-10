package org.marcjan.karol.chess_standoff

import org.scalatest.{Matchers, FlatSpec}

class PieceSpec extends FlatSpec with Matchers {

  /**
   * Returns a list of all displacements with the x and y deltas in the given
   * ranges.
   *
   * @param ofX range of the x displacement
   * @param ofY range of the y displacement
   * @return list of all displacements with the x and y components in the given
   *         ranges
   */
  def displacementsWithRanges(ofX: Range, ofY: Range): List[(Int, Int)] =
    ofX.toList.flatMap((xDelta) => ofY.toList.map((yDelta) => (xDelta, yDelta)))

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

  "A Queen" should "be able to move n squares in any direction" in {
    val queenValidMoves = displacementsWithRanges(-8 to 8, -8 to 8) filter {
      case (xDelta, yDelta) => xDelta == yDelta && xDelta != 0
    }

    queenValidMoves map { Queen.canMoveBy(_) should be (true)}
  }
}
