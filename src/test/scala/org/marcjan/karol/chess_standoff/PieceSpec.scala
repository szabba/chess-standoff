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

  /**
   * Returns true when the given displacement is diagonal and false otherwise.
   * It is left unspecified for the zero displacement.
   *
   * @param displacement a displacement as a tuple of (xDelta, yDelta)
   * @return is the displacement diagonal?
   */
  def isDiagonal(displacement: (Int, Int)): Boolean = {
    displacement match { case (xDelta, yDelta) => xDelta == yDelta }
  }

  /**
   * Returns true when the given displacement is along the board's edge. It is
   * left unspecified for the zero displacement.
   *
   * @param displacement a displacement as a tuple of (xDelta, yDelta)
   * @return is the displacement along the board's edge?
   */
  def isAlongBoardEdge(displacement: (Int, Int)): Boolean = {
    displacement match {
      case (_, 0) => true
      case (0, _) => true
      case _ => false
    }
  }

  "A King" should "be able to move by one square in any direction" in {
    val kingsValidMoves = displacementsWithRanges(-1 to 1, -1 to 1) filter {
      case (xDelta, yDelta) => math.abs(xDelta) == 1 || math.abs(yDelta) == 1
    }

    kingsValidMoves foreach { King.canMoveBy(_) should be (true) }
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
    val queenDiagMoves = displacementsWithRanges(-8 to 8, -8 to 8) filter(isDiagonal)

    queenDiagMoves foreach { Queen.canMoveBy(_) should be (true)}
  }

  it should "not be able to move non-diagonally" in {
    val queenInvalidMoves = displacementsWithRanges(-8 to 8, -8 to 8) filter {
      case (xDelta, yDelta) => xDelta != yDelta
    }

    queenInvalidMoves foreach { Queen.canMoveBy(_) should be (false) }
  }

  "A Rook" should "be able to move n squares along the board's edge" in {
    val rookValidMoves = displacementsWithRanges(-8 to 8, -8 to 8) filter {
      case (_, 0) => true
      case (0, _) => true
      case _ => false
    }

    rookValidMoves foreach { Rook.canMoveBy(_) should be (true) }
  }

  it should "not be able to move unless the displacements is along the board's edge" in {
    val rookInvalidMoves = displacementsWithRanges(-8 to 8, -8 to 8) filter {
      case (_, 0) => false
      case (0, _) => false
      case _ => true
    }

    rookInvalidMoves foreach { Rook.canMoveBy(_) should be (false) }
  }
}
