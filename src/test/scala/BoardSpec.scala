import org.marcjan.karol.chess_standoff.{Board, UnitSpec}

/**
 * Spec for the chessboard.
 */
class BoardSpec extends UnitSpec {

  "A Board" should "have given number of rows" in {
    val rows = 7
    val cols = 6

    val board = new Board(rows, cols)

    board.rows should be (rows)
  }
}
