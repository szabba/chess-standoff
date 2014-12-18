package org.marcjan.karol.chess_standoff

/**
 * Spec for board symmetries.
 */
class SymmetrySpec extends UnitSpec {

  "The Identity symmetry" should "not change position components" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)

    Identity(rows, columns, position) should be (position)
  }

  "The MirrorRows symmetry" should "not change position row" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)
    val transformed = MirrorRows(rows, columns, position)

    transformed.row should be (2)
  }

  it should "flip column values in a row" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)
    val transformed = MirrorRows(rows, columns, position)

    transformed.column should be (0)
  }
}
