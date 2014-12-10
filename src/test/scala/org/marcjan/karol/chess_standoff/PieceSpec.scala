package org.marcjan.karol.chess_standoff

import org.scalatest.{Matchers, FlatSpec}

class PieceSpec extends FlatSpec with Matchers {

  import Displacement.{isDiagonal, isAlongBoardEdge}

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

  val standardChessboardMoves = displacementsWithRanges(-8 to 8, -8 to 8)

  val diagonalMoves = standardChessboardMoves filter (isDiagonal)

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
    diagonalMoves foreach { Queen.canMoveBy(_) should be (true)}
  }

  it should "be able to move n squares along the board's edge" in {
    val queenMovesAlongEdge = displacementsWithRanges(-8 to 8, -8 to 8) filter(
      isAlongBoardEdge)

    queenMovesAlongEdge foreach { Queen.canMoveBy(_) should be (true) }
  }

  it should "not be able to move unless the displacement is diagonal or along the board's edge" in {
    val queenInvalidMoves = displacementsWithRanges(-8 to 8, -8 to 8) filter(
      (displacement) => {
        if (isDiagonal(displacement)) false
        else if (isAlongBoardEdge(displacement)) false
        else true
      })

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

  "A Bishop" should "be able to move n squares diagonally in any direction" in {
    diagonalMoves foreach {
      Bishop.canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move non-diagonally" in {
    standardChessboardMoves filter (!isDiagonal(_)) foreach {
      Bishop.canMoveBy(_) should be (false)
    }
  }

  val knightMoves = List(
    (2, 1), (2, -1), (-2, 1), (-2, -1),
    (1, 2), (1, -2), (-1, 2), (-1, -2)
  )

  "A Knight" should "be able to move by two squares along one edge and one along the other" in {
    knightMoves foreach {
      Knight.canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move unless the displacement is two squares along one edge and one along the other" in {
    standardChessboardMoves filter {
      knightMoves contains _
    } foreach {
      Knight.canMoveBy(_) should be (false)
    }
  }
}
