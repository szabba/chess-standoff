package org.marcjan.karol.chess_standoff

/**
 * Spec for the chessboard.
 */
class BoardSpec extends UnitSpec {

  "A Board" should "have given number of rows" in {
    val rows = 7
    val columns = 6

    val board = Board(rows, columns)

    board.rows should be (rows)
  }

  it should "have given number of columns" in {
    val rows = 7
    val columns = 6

    val board = Board(rows, columns)

    board.columns should be (columns)
  }

  it should "contain no pieces unless some are given" in {
    val rows = 7
    val columns = 6

    val board = Board(rows, columns)

    board.pieces.isEmpty should be (true)
  }

  it should "contain a piece when given one" in {
    val rows = 7
    val columns = 6

    val rook = Rook(Position(6, 3))
    val board = Board(rows, columns, List(rook))

    board.pieces.contains(rook) should be (true)
  }

  it should "contain each piece it's given" in {
    val rows = 7
    val cols = 6

    val pieces = List(
      new King(Position(0, 0)), new Queen(Position(2, 3)), new Rook(Position(6, 5)))

    val board = Board(rows, cols, pieces)

    pieces foreach {
      board.pieces.contains(_) should be (true)
    }
  }

  def positionsOn(board: Board) =
    0 until board.rows flatMap (row =>
        0 until board.columns map (column =>
          Position(row, column)))

  it should "have all positions safe when it contains no pieces" in {
    val rows = 7
    val cols = 6

    val board = Board(rows, cols)

    positionsOn(board) foreach {
      board.safePositions.contains(_)
    }
  }

  it should "consider all positions a piece can move to as unsafe" in {
    val rows = 7
    val cols = 6

    val queen = Queen(Position(6, 5))

    val board = Board(rows, cols, List(queen))

    positionsOn(board) foreach (position =>
      if (queen.canMoveTo(position))
        board.safePositions.contains(position) should be (false))
  }

  it should "know that a piece can be safely put anywhere on it when " ++
    "it's empty" in {

    val board = Board(1, 5)

     positionsOn(board) forall (
       board.safePlacesFor(King) contains _
      ) should be (true)
  }

  it should "know that a new piece can only be placed safeky where an old " ++
    "one cannot move" in {

    val rook = Rook(Position(2, 2))
    val board = Board(3, 3, List(rook))

    val canMoveNowhere = new PieceKind {
      /**
       * Returns true if the object can move by the given displacement and false
       * otherwise. The result for a zero displacement is left unspecified.
       *
       * @param move the displacement as a tuple of (xDelta, yDelta)
       * @return can the object be moved by the given displacement?
       */
      override def canMoveBy(move: Move): Boolean = false
    }

    board.safePlacesFor(canMoveNowhere) forall (! rook.canMoveTo(_)
      ) should be (true)
  }

  it should "know that a new piece can only be placed safely in a place " ++
    "from which it cannot reach an old one" in {

    val rook = Rook(Position(2, 2))
    val board = Board(5, 5, List(rook))

    board.safePlacesFor(Bishop).filter(safePlace =>
      Bishop(safePlace).canMoveTo(rook.position)
      ).isEmpty should be (true)
  }
}