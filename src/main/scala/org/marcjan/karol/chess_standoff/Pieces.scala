package org.marcjan.karol.chess_standoff

/**
 * A chess piece.
 */
sealed abstract class Piece {
  /**
   * Returns true if the Piece can move by the given displacement and false
   * otherwise.
   *
   * @param positionDelta the displacement as a tuple of (xDelta, yDelta)
   * @return can the Piece be moved by the given displacement?
   */
  def canMoveBy(positionDelta: (Int, Int)): Boolean
}

/**
 * A King piece. It can move in any of the eight directions but only by one
 * square.
 */
object King extends Piece {
  override def canMoveBy(positionDelta: (Int, Int)): Boolean =
    positionDelta match {
      case (xDelta, yDelta) =>
        (-1 to 1 contains xDelta) &&
        (-1 to 1 contains yDelta) &&
        (xDelta != 0 || yDelta != 0)
    }
}

/**
 * A Queen piece. It can move along a straight line in any of the eight
 * directions by any number of squares.
 */
object Queen extends Piece {
  override def canMoveBy(positionDelta: (Int, Int)): Boolean = false
}

/**
 * A Rook piece. It can move along a straight line in the four non-diagonal
 * directions by any number of squares.
 */
object Rook extends Piece {
  override def canMoveBy(positionDelta: (Int, Int)): Boolean = false
}

/**
 * A Bishop piece. It can move along a straight line in the four diagonal
 * directions by any number of squares.
 */
object Bishop extends Piece {
  override def canMoveBy(positionDelta: (Int, Int)): Boolean = false
}

/**
 * A Knight piece. It moves by two squares in one of the non-diagonal
 * directions and then by two squares in a direction perpendicular to the first
 * one, tracing the letter L's shape.
 */
object Knight extends Piece {
  override def canMoveBy(positionDelta: (Int, Int)): Boolean = false
}