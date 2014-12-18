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
}
