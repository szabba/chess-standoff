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

  it should "have the given column" in {
    val row = 7
    val column = 3

    val position = new Position(row, column)

    position.column should be (column)
  }

  it should "have the same row whether created with or without new" in {
    val row = 7
    val column = 3

    val somePosition = new Position(row, column)
    val otherPosition = Position(row, column)

    otherPosition.row should be (somePosition.row)
  }

  it should "have the same column whether created with or without new" in {
    val row = 7
    val column = 3

    val somePosition = new Position(row, column)
    val otherPosition = Position(row, column)

    otherPosition.column should be (somePosition.column)
  }
}
