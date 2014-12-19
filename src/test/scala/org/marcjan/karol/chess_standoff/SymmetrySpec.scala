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

  "The MirrorColumns symmetry" should "not change position columns" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)
    val transformed = MirrorColumns(rows, columns, position)

    transformed.column should be (3)
  }

  it should "flip row values in a column" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)
    val transformed = MirrorColumns(rows, columns, position)

    transformed.row should be (4)
  }

  "The RotateRightBy180Degrees symmetry" should "flip row values in a column" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)
    val transformed = RotateRightBy180Degrees(rows, columns, position)

    transformed.row should be (4)
  }

  it should "flip column values in a row" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)
    val transformed = RotateRightBy180Degrees(rows, columns, position)

    transformed.column should be (0)
  }

  "The MirrorDiagonal symmetry" should "not change positions on the main diagonal" in {

    val edgeLength = 7

    val position = Position(2, 2)
    val transformed = MirrorDiagonal(edgeLength, edgeLength, position)

    transformed should be (position)
  }

  it should "swap position coordinates" in {

    val edgeLegth = 7

    val position = Position(1, 3)
    val transformed = MirrorDiagonal(edgeLegth, edgeLegth, position)

    transformed.row should be (position.column)
    transformed.column should be (position.row)
  }

  "The MirrorAntidiagonal symmetry" should "not change positions on the " +
    "antidiagonal" in {

    val edgeLength = 7

    val position = Position(1, 5)
    val transformed = MirrorAntidiagonal(edgeLength, edgeLength, position)

    transformed should be (position)
  }

  it should "mirror positions with respect to the antidiagonal" in {

    val edgeLength = 7

    val position = Position(1, 2)
    val transformed = MirrorAntidiagonal(edgeLength, edgeLength, position)

    transformed.row should be (4)
    transformed.column should be (5)
  }
}