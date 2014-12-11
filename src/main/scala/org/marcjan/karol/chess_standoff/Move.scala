package org.marcjan.karol.chess_standoff

/**
 * A displacement between two squares on a chessboard.
 *
 * @param xDelta displacement along the horizontal edge
 * @param yDelta displacement along the vertical edge
 */
class Move(val xDelta: Int, val yDelta: Int) {

  override def toString: String =
    "Move(" ++ xDelta.toString ++ ", " ++ yDelta.toString ++ ")"

  /**
   * Returns true when the two moves are equal.
   *
   * @param move move to compare against
   * @return are the two moves equal?
   */
  def ==(move: Move) =
    xDelta == move.xDelta && yDelta == move.yDelta
  
  /**
   * Returns true when the move is diagonal. Left unspecified when isZero is
   * true.
   *
   * @return is the move diagonal?
   */
  def isDiagonal: Boolean =
    xDelta == yDelta

  /**
   * Returns true when the move is along the board's edge. Left unspecified when
   * isZero is true.
   *
   * @return is the move along an edge of the board?
   */
  def isAlongBoardEdge: Boolean =
    xDelta == 0 || yDelta == 0

  /**
   * Returns true when the move is zero, ie wouldn't shift a position.
   *
   * @return is the move zero?
   */
  def isZero: Boolean =
    xDelta == 0 && yDelta == 0
}

/**
 * Object containing functions that deal with displacements/chess moves
 * represented as Int tuples.
 */
object Move {

  /**
   * Shortcut for creating Moves without the new keyword.
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
  def movesExcept(from: List[Move], movesToExclude: List[Move]) = {
    def keep(move: Move) =
      !(movesToExclude exists {
        _ == move
      })

    from filter keep
  }
}