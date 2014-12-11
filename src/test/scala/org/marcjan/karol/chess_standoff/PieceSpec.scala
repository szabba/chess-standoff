package org.marcjan.karol.chess_standoff

import org.scalatest.{FlatSpec, Matchers}

/**
 * Spec for chess pieces.
 */
class PieceSpec extends UnitSpec {

  import Moves.{isAlongBoardEdge, isDiagonal, isZero}

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

  val standardChessboardMoves = displacementsWithRanges(-8 to 8, -8 to 8) filter (isZero)

  val diagonalMoves = standardChessboardMoves filter (isDiagonal)

  val movesAlongBoardEdge = standardChessboardMoves filter (isAlongBoardEdge)

  /**
   * Returns a list of all the moves possible on a standard chessboard except
   * for the one's in the argument list.
   * @param movesToExclude list of moves to exclude
   * @return moves possible on a standard chessboard except for those present in
   *         the argument
   */
  def movesExcept(movesToExclude: List[(Int, Int)]) =
    Moves.movesExcept(standardChessboardMoves, movesToExclude)

  val kingMoves = displacementsWithRanges(-1 to 1, -1 to 1) filter (isZero)

  "A King" should "be able to move by one square in any direction" in {

    kingMoves foreach { King.canMoveBy(_) should be (true) }
  }

  it should "not be able to move beyond one square in any direction" in {
    movesExcept(kingMoves) foreach {
      King.canMoveBy(_) should be (false)
    }
  }

  "A Queen" should "be able to move n squares diagonally in any direction" in {
    diagonalMoves foreach { Queen.canMoveBy(_) should be (true)}
  }

  it should "be able to move n squares along the board's edge" in {
    movesAlongBoardEdge foreach { Queen.canMoveBy(_) should be (true) }
  }

  it should "not be able to move unless the displacement is diagonal or along the board's edge" in {
    movesExcept(diagonalMoves ++ movesAlongBoardEdge) foreach {
      Queen.canMoveBy(_) should be (false)
    }
  }

  "A Rook" should "be able to move n squares along the board's edge" in {
    movesAlongBoardEdge foreach { Rook.canMoveBy(_) should be (true) }
  }

  it should "not be able to move unless the displacements is along the board's edge" in {
    movesExcept(movesAlongBoardEdge) foreach { Rook.canMoveBy(_) should be (false) }
  }

  "A Bishop" should "be able to move n squares diagonally in any direction" in {
    diagonalMoves foreach {
      Bishop.canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move non-diagonally" in {
    movesExcept(diagonalMoves) foreach {
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
    movesExcept(knightMoves) foreach {
      Knight.canMoveBy(_) should be (false)
    }
  }
}
