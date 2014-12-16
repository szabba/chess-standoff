package org.marcjan.karol.chess_standoff

import org.scalatest.words.CanVerb

/**
 * Spec for chess pieces.
 */
class PieceSpec extends UnitSpec with CanVerb {

  /**
   * Returns a list of all displacements with the x and y deltas in the given
   * ranges.
   *
   * @param ofX range of the x displacement
   * @param ofY range of the y displacement
   * @return list of all displacements with the x and y components in the given
   *         ranges
   */
  def displacementsWithRanges(ofX: Range, ofY: Range): List[Move] =
    ofX.toList.flatMap(xDelta => ofY.toList.map(yDelta => Move(xDelta, yDelta)))

  val standardChessboardMoves =
    displacementsWithRanges(-8 to 8, -8 to 8) filter { _.isZero }

  val diagonalMoves = standardChessboardMoves filter { _.isDiagonal }

  val movesAlongBoardEdge = standardChessboardMoves filter { _.isAlongBoardEdge }

  printf("diagonals: %s\n\n", diagonalMoves)
  printf("edge-alongs: %s\n\n", movesAlongBoardEdge)

  val dummyPosition = Position(0, 0)

  /**
   * Returns a list of all the moves possible on a standard chessboard except
   * for the one's in the argument list.
   * @param movesToExclude list of moves to exclude
   * @return moves possible on a standard chessboard except for those present in
   *         the argument
   */
  def movesExcept(movesToExclude: List[Move]) =
    standardChessboardMoves filterNot (move =>
      movesToExclude exists { _ == move })

  val kingMoves = displacementsWithRanges(-1 to 1, -1 to 1) filter (_.isZero)

  "A King" should "be able to move by one square in any direction" in {

    kingMoves foreach {
      King(dummyPosition).canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move beyond one square in any direction" in {
    movesExcept(kingMoves) foreach {
      King(dummyPosition).canMoveBy(_) should be (false)
    }
  }

  "A Queen" should "be able to move n squares diagonally in any direction" in {
    diagonalMoves foreach {
      Queen(dummyPosition).canMoveBy(_) should be (true)
    }
  }

  it should "be able to move n squares along the board's edge" in {
    movesAlongBoardEdge foreach {
      Queen(dummyPosition).canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move unless the displacement is diagonal or " ++
    "along the board's edge" in {

    movesExcept(diagonalMoves ++ movesAlongBoardEdge) foreach {
      Queen(dummyPosition).canMoveBy(_) should be (false)
    }
  }

  "A Rook" should "be able to move n squares along the board's edge" in {
    movesAlongBoardEdge foreach {
      Rook(dummyPosition).canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move unless the displacements is along the " ++
    "board's edge" in {

    movesExcept(movesAlongBoardEdge) foreach {
      Rook(dummyPosition).canMoveBy(_) should be (false)
    }
  }

  "A Bishop" should "be able to move n squares diagonally in any direction" in {
    diagonalMoves foreach {
      Bishop(dummyPosition).canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move non-diagonally" in {
    movesExcept(diagonalMoves) foreach {
      Bishop(dummyPosition).canMoveBy(_) should be (false)
    }
  }

  val knightMoves = List(
    Move(2, 1), Move(2, -1), Move(-2, 1), Move(-2, -1),
    Move(1, 2), Move(1, -2), Move(-1, 2), Move(-1, -2)
  )

  "A Knight" should "be able to move by two squares along one edge and one " ++
    "along the other" in {

    knightMoves foreach {
      Knight(dummyPosition).canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move unless the displacement is two squares " ++
    "along one edge and one along the other" in {

    movesExcept(knightMoves) foreach {
      Knight(dummyPosition).canMoveBy(_) should be (false)
    }
  }

  "A Piece" can "only move to a position if it can move by the end " ++
    "position's difference with the piece's position" in {

    val pieces = List(
      King(dummyPosition), Queen(dummyPosition),
      Rook(dummyPosition), Bishop(dummyPosition),
      Knight(dummyPosition))

    pieces foreach (piece => {
      standardChessboardMoves foreach (move => {

        piece.canMoveTo(piece.position + move) should be (piece.canMoveBy(move))
      })
    })
  }
}