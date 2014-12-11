package org.marcjan.karol.chess_standoff

/**
 * A position of a square on the chessboard.
 *
 * @param row row containing the square
 * @param column column containing the square
 */
class Position(val row: Int, val column: Int)

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
  def apply(row: Int, column: Int): Position = {
    new Position(0, 0)
  }
}
