package org.marcjan.karol.chess_standoff

import scala.collection.mutable.HashMap

/**
 * A position of a square on the chessboard. The rows are counted (unlike
 * normally in chess) top to bottom, and the columns are counted left to right.
 *
 * @param row row containing the square
 * @param column column containing the square
 */
class Position(val row: Int, val column: Int) {

  /**
   * Returns true if the value is equal to p.
   *
   * @param p position to compare to
   * @return are the positions equal?
   */
  def ==(p: Position): Boolean =
    row == p.row && column == p.column

  override def equals(p: Any): Boolean =
    p match {
      case p: Position => this == p
      case _ => false
    }

  override def toString: String =
    "Position(" ++ row.toString ++ ", " ++ column.toString ++ ")"

  /**
   * Returns a move <em>m</em> such that <em>this + m == somewhere</em>.
   *
   * @param somewhere
   * @return
   */
  def -(somewhere: Position): Move = Move(
    column - somewhere.column,
    row - somewhere.row)

  /**
   * Returns the position shifted by the given move.
   *
   * @param move move to shift by
   * @return position shifted by the given move
   */
  def +(move: Move): Position = Position(
    row + move.yDelta,
    column + move.xDelta)
}

/**
 * Associate object of Position class.
 */
object Position {

  private val cache: HashMap[Int, HashMap[Int, Position]] =
    new HashMap[Int, HashMap[Int, Position]]()

  /**
   * Shortcut for creating Positions without the new keyword.
   *
   * @param row row containing the square
   * @param column column containing the square
   * @return a position
   */
  def apply(row: Int, column: Int): Position =
    cache.getOrElseUpdate(
      row, new HashMap[Int, Position]()
    ).getOrElseUpdate(
      column, new Position(row, column))
}
