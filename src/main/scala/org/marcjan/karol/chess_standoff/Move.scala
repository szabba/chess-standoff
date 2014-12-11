package org.marcjan.karol.chess_standoff

/**
 * Typesafe representation for a displacement on a chessboard.
 *
 * @param xDelta displacement along the horizontal edge
 * @param yDelta displacement along the vertical edge
 */
class Move(val xDelta: Int, val yDelta: Int)

/**
 * Object containing functions that deal with displacements/chess moves
 * represented as Int tuples.
 */
object Move {

  /**
   * Shortand for creating Moves without the new keyword.
   *
   * @param xDelta displacement along the horizontal edge
   * @param yDelta displacement along the vertical edge
   * @return a move
   */
  def apply(xDelta: Int, yDelta: Int): Move =
    new Move(xDelta, yDelta)

  /**
   * Returns true when the given displacement is diagonal and false otherwise.
   * It is left unspecified for the zero displacement.
   *
   * @param displacement a displacement as a tuple of (xDelta, yDelta)
   * @return is the displacement diagonal?
   */
  def isDiagonal(displacement: (Int, Int)): Boolean =
    displacement match {
      case (xDelta, yDelta) => xDelta == yDelta
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

  /**
   * Returns true when both components of the displacement are zero.
   *
   * @param displacement a displacement as a tuple of (xDelta, yDelta)
   * @return is the displacement zero?
   */
  def isZero(displacement: (Int, Int)): Boolean = displacement == (0, 0)

  /**
   * Returns a list of all the moves in the first list except for those in the
   * second one.
   *
   * @param from list of moves to choose from
   * @param movesToExclude list of moves to exclude
   * @return moves in the first list but not the second
   */
  def movesExcept(from: List[(Int, Int)], movesToExclude: List[(Int, Int)]) = {
    from filter (
      (move) => { ! (movesToExclude contains move) }
    )
  }
}
