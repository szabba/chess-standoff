package org.marcjan.karol.chess_standoff

/**
 * Spec for chessboard positions.
 */
class PositionSpec extends UnitSpec {

  "A Position" should "have the given row" in {
    val row = 7
    val column = 3

    val position = new Position(row, column)

    position.row should be (row)
  }
}
