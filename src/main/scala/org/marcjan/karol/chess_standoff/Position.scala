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

  /**
   * Returns the position shifted by the given move.
   *
   * @param move move to shift by
   * @return position shifted by the given move
   */
  def +(move: Move): Position = Position(
    row + move.yDelta, 0)
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
