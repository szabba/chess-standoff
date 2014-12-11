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

  it should "be equal to itself" in {
    val somePosition = Position(7, 3)

    somePosition == somePosition should be (true)
  }

  it should "not be equal to a position within a different row" in {
    val column = 3

    val somePosition = Position(7, column)
    val otherPosition = Position(4, column)

    somePosition == otherPosition should be (false)
  }

  it should "not be equal to a position within a different column" in {
    val row = 7

    val somePosition = Position(row, 3)
    val otherPosition = Position(row, 4)

    somePosition == otherPosition should be (false)
  }
}
