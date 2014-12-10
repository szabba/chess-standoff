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
    val kingsValidMoves = displacementsWithRanges(-1 to 1, -1 to 1) filter {
      case (xDelta, yDelta) => math.abs(xDelta) == 1 || math.abs(yDelta) == 1
    }

    kingsValidMoves foreach {
      King.canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move beyond one square in any direction" in {
    val outOfKingsRange = displacementsWithRanges(-2 to 2, -2 to 2) filter {
      case (xDelta, yDelta) => xDelta == 2 || yDelta == 2
    }

    outOfKingsRange foreach {
      King.canMoveBy(_) should be (false)
    }
  }

  "A Queen" should "be able to move n squares diagonally in any direction" in {
    val queenValidMoves = displacementsWithRanges(-8 to 8, -8 to 8) filter {
      case (xDelta, yDelta) => xDelta == yDelta && xDelta != 0
    }

    queenValidMoves foreach { Queen.canMoveBy(_) should be (true)}
  }
}
