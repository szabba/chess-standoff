package org.marcjan.karol.chess_standoff

/**
 * A position of a square on the chessboard.
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

  /**
   * Shortcut for creating Positions without the new keyword.
   *
   * @param row row containing the square
   * @param column column containing the square
   * @return a position
   */
  def apply(row: Int, column: Int): Position =
    new Position(row, column)
}
