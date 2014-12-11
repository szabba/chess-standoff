package org.marcjan.karol.chess_standoff

/**
 * A chess piece.
 */
sealed abstract class Piece {
  /**
   * Returns true if the Piece can move by the given displacement and false
   * otherwise. The result for a zero displacement is left unspecified.
   *
   * @param move the displacement as a tuple of (xDelta, yDelta)
   * @return can the Piece be moved by the given displacement?
   */
  def canMoveBy(move: Move): Boolean
}

/**
 * A King piece. It can move in any of the eight directions but only by one
 * square.
 */
object King extends Piece {
  override def canMoveBy(move: Move): Boolean =
    (-1 to 1 contains move.xDelta) && (-1 to 1 contains move.yDelta)
}

/**
 * A Queen piece. It can move along a straight line in any of the eight
 * directions by any number of squares.
 */
object Queen extends Piece {
  override def canMoveBy(move: Move): Boolean =
    move.yDelta == 0 || move.xDelta == 0 || move.xDelta == move.yDelta
}

/**
 * A Rook piece. It can move along a straight line in the four non-diagonal
 * directions by any number of squares.
 */
object Rook extends Piece {
  override def canMoveBy(move: Move): Boolean =
    move.xDelta == 0 || move.yDelta == 0
}

/**
 * A Bishop piece. It can move along a straight line in the four diagonal
 * directions by any number of squares.
 */
object Bishop extends Piece {
  override def canMoveBy(move: Move): Boolean =
    move.xDelta == move.yDelta
}

/**
 * A Knight piece. It moves by two squares in one of the non-diagonal
 * directions and then by two squares along one edge of the board and one along
 * the other, tracing the shape of the letter 'L'.
 */
object Knight extends Piece {
  override def canMoveBy(move: Move): Boolean = {
    val components = List(move.xDelta, move.yDelta)
    val absComponents = components map (math.abs) sortWith (_ < _)

    absComponents match {
      case List(1, 2) => true
      case _ => false
    }
  }
}