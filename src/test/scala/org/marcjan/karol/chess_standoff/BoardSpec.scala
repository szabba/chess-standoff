package org.marcjan.karol.chess_standoff

/**
 * Spec for the chessboard.
 */
class BoardSpec extends UnitSpec {

  "A Board" should "have given number of rows" in {
    val rows = 7
    val columns = 6

    val board = new Board(rows, columns)

    board.rows should be (rows)
  }

  it should "have given number of columns" in {
    val rows = 7
    val columns = 6

    val board = new Board(rows, columns)

    board.columns should be (columns)
  }
}