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

  val consideredMoves =
    (-8 to 8).flatMap(xDelta =>

        (-8 to 8).map(yDelta =>

          Move(xDelta, yDelta))

    ).filterNot( _.isZero )


  val diagonalMoves = consideredMoves filter { _.isDiagonal }

  val movesAlongBoardEdge = consideredMoves filter { _.isAlongBoardEdge }

  val kingMoves = displacementsWithRanges(-1 to 1, -1 to 1) filter (_.isZero)

  "A King" should "be able to move by one square in any direction" in {

    List(
    Move(-1, -1), Move(0, -1), Move(1, -1),
    Move(-1,  0),              Move(1,  0),
    Move(-1,  1), Move(0,  1), Move(1,  1)
    ) foreach {
      King.canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move beyond one square in any direction" in {

    List(
    Move(-2, -2), Move(-1, -2), Move(0, -2), Move(1, -2), Move(2, -2),
    Move(-2, -1),                                         Move(2, -1),
    Move(-2,  0),                                         Move(2,  0),
    Move(-2,  1),                                         Move(2,  1),
    Move(-2,  2), Move(-1,  2), Move(0,  2), Move(1,  2), Move(2,  2)
    ) foreach {
      King.canMoveBy(_) should be (false)
    }
  }

  "A Queen" should "be able to move n squares diagonally in any direction" in {
    diagonalMoves foreach {
      Queen.canMoveBy(_) should be (true)
    }
  }

  it should "be able to move n squares along the board's edge" in {
    movesAlongBoardEdge foreach {
      Queen.canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move unless the displacement is diagonal or " ++
  "along the board's edge" in {

    consideredMoves filterNot {
      case move => move.isDiagonal || move.isAlongBoardEdge
    } foreach {
      Queen.canMoveBy(_) should be (false)
    }
  }

  "A Rook" should "be able to move n squares along the board's edge" in {
    movesAlongBoardEdge foreach {
      Rook.canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move unless the displacements is along the " ++
  "board's edge" in {

    consideredMoves filterNot {
      _.isAlongBoardEdge
    } foreach {
      Rook.canMoveBy(_) should be (false)
    }
  }

  "A Bishop" should "be able to move n squares diagonally in any direction" in {
    diagonalMoves foreach {
      Bishop.canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move non-diagonally" in {

    consideredMoves filterNot {
      _.isDiagonal
    } foreach {
      case move =>
        Bishop.canMoveBy(move) should be (false)
    }
  }

  val knightMoves = List(
    Move(2, 1), Move(2, -1), Move(-2, 1), Move(-2, -1),
    Move(1, 2), Move(1, -2), Move(-1, 2), Move(-1, -2)
  )

  "A Knight" should "be able to move by two squares along one edge and one " ++
  "along the other" in {

    knightMoves foreach {
      Knight.canMoveBy(_) should be (true)
    }
  }

  it should "not be able to move unless the displacement is two squares " ++
  "along one edge and one along the other" in {

    consideredMoves filterNot {
      knightMoves.contains(_)
    } foreach {
      Knight.canMoveBy(_) should be (false)
    }
  }

  val dummyPosition = Position(0, 0)

  "A Piece" can "only move to a position if it can move by the end " ++
  "position's difference with the piece's position" in {


    val pieces = List(
      King(dummyPosition), Queen(dummyPosition),
      Rook(dummyPosition), Bishop(dummyPosition),
      Knight(dummyPosition))
    pieces foreach (piece => {
      consideredMoves foreach (move => {

        piece.canMoveTo(piece.position + move) should be (piece.kind.canMoveBy(move))
      })
    })
  }
}