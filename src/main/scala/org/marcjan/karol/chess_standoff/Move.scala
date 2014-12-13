package org.marcjan.karol.chess_standoff

import scala.collection.mutable.HashMap

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

  private val cache: HashMap[Int, HashMap[Int, Move]] =
    new HashMap[Int, HashMap[Int, Move]]()

  /**
   * Shortcut for creating Moves without the new keyword.
   *
   * @param xDelta displacement along the horizontal edge
   * @param yDelta displacement along the vertical edge
   * @return a move
   */
  def apply(xDelta: Int, yDelta: Int): Move =
    cache.getOrElseUpdate(
      xDelta, new HashMap[Int, Move]()
    ).getOrElseUpdate(
        yDelta, new Move(xDelta, yDelta))
}