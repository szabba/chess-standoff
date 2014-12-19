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

  it should "cancel out with it's inverse" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)
    val transformed = Identity.inverse(rows, columns,
      Identity(rows, columns, position))

    transformed should be (position)
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

  it should "cancel out with it's inverse" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)
    val transformed = MirrorRows.inverse(rows, columns,
      MirrorRows(rows, columns, position))

    transformed should be (position)
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

  it should "cancel out with it's inverse" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)
    val transformed = MirrorColumns.inverse(rows, columns,
      MirrorColumns(rows, columns, position))

    transformed should be (position)
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

  it should "cancel out with it's inverse" in {

    val rows = 7
    val columns = 4

    val position = Position(2, 3)
    val transformed = RotateRightBy180Degrees.inverse(rows, columns,
      RotateRightBy180Degrees(rows, columns, position))

    transformed should be (position)
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

  "The RotateRightBy90Degrees symmetry" should "put the (0, 0) position at " +
    "the 0-th column of the last row" in {

    val edgeLength = 7

    val position = Position(0, 0)
    val transformed = RotateRightBy90Degrees(edgeLength, edgeLength, position)

    transformed.column should be (0)
    transformed.row should be (edgeLength - 1)
  }

  it should "be invrariant with respect to the middle position on an " +
    "odd-edge square board" in {

    val edgeLength = 7
    val edgeMiddle = (edgeLength - 1) / 2

    val position = Position(edgeMiddle, edgeMiddle)
    val transformed = RotateRightBy90Degrees(edgeLength, edgeLength, position)

    transformed should be (position)
  }

  it should "transform the lower left corner into the lower right one" in {

    val edgeLength = 7

    val position = Position(edgeLength - 1, 0)
    val transformed = RotateRightBy90Degrees(edgeLength, edgeLength, position)

    transformed should be (Position(edgeLength - 1, edgeLength - 1))
  }

  "The RotateRightBy270Degrees symmetry" should "be invariant with respect " +
    "to the middle position on an odd-edge square board" in {

    val edgeLength = 7
    val edgeMiddle = (edgeLength - 1) / 2

    val position = Position(edgeMiddle, edgeMiddle)
    val transformed = RotateRightBy270Degrees(edgeLength, edgeLength, position)

    transformed should be (position)
  }

  it should "put the (0, 0) position at the last column of the 0-th row" in {

    val edgeLength = 7

    val position = Position(0, 0)
    val transformed = RotateRightBy270Degrees(edgeLength, edgeLength, position)

    println(position)
    println(transformed)
    transformed.row should be (0)
    transformed.column should be (edgeLength - 1)
  }

  it should "transform the lower left corner into the upper left one" in {

    val edgeLength = 7

    val position = Position(edgeLength - 1, 0)
    val transformed = RotateRightBy270Degrees(edgeLength, edgeLength, position)

    transformed should be (Position(0, 0))
  }
}