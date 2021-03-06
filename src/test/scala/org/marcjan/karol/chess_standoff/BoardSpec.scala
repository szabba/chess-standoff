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
      King(Position(0, 0)), Queen(Position(2, 3)), Rook(Position(6, 5)))

    val board = Board(rows, cols, pieces)

    pieces foreach {
      board.pieces.contains(_) should be (true)
    }
  }

  def positionsOn(board: Board) =
    0 until board.rows flatMap (row =>
        0 until board.columns map (column =>
          Position(row, column)))

  "safeAt" should "hold for all positions and piece kinds on an empty board" in {

    val board = Board(4, 3)

    positionsOn(board) forall (position =>
      List(King, Queen, Rook, Bishop, Knight) forall (pieceKind =>
        board.safeAt(position, pieceKind))
      ) should be (true)
  }

  it should "not hold for positions where an existing piece could move" in {

    val knight = Knight(Position(2, 2))
    val board = Board(5, 5, List(knight))

    board.safeAt(Position(0, 1), CanMoveNowhere) should be (false)
    board.safeAt(Position(0, 3), CanMoveNowhere) should be (false)
    board.safeAt(Position(1, 0), CanMoveNowhere) should be (false)
    board.safeAt(Position(1, 4), CanMoveNowhere) should be (false)
    board.safeAt(Position(3, 0), CanMoveNowhere) should be (false)
    board.safeAt(Position(3, 4), CanMoveNowhere) should be (false)
    board.safeAt(Position(4, 1), CanMoveNowhere) should be (false)
    board.safeAt(Position(4, 3), CanMoveNowhere) should be (false)
  }

  it should "not hold for positions where a piece already resides" in {
    val knight = Knight(Position(2, 2))
    val king = King(Position(0, 0))
    val board = Board(5, 5, List(king, knight))

    List(King, Queen, Rook, Bishop, Knight) exists (candidateKind =>
      board.pieces exists (existingPiece =>
        board.safeAt(existingPiece.position, candidateKind)
        )) should be (false)
  }

  it should "not hold for positions from which the new piece could move to " ++
    "a position held by an existing one" in {

    val bishop = Bishop(Position(2, 2))
    val board = Board(5, 5, List(bishop))

    board.safeAt(Position(0, 2), Rook) should be (false)
    board.safeAt(Position(1, 2), Rook) should be (false)

    board.safeAt(Position(3, 2), Rook) should be (false)
    board.safeAt(Position(4, 2), Rook) should be (false)

    board.safeAt(Position(2, 0), Rook) should be (false)
    board.safeAt(Position(2, 1), Rook) should be (false)

    board.safeAt(Position(2, 3), Rook) should be (false)
    board.safeAt(Position(2, 4), Rook) should be (false)
  }

  "findSafePlacement" should "contain only empty boards when given no " ++
    "pieces" in {

    val boards = Board.findSafePlacement(3, 4)

    boards.forall(_.pieces.isEmpty) should be (true)
  }

  it should "contain only one board when given no pieces" in {

    val boards = Board.findSafePlacement(3, 4)

    boards.length should be (1)
  }

  it should "contain as many boards as there are positions on each board " ++
    "when given a single piece kind" in {

    val rows = 3
    val columns = 4

    val boards = Board.findSafePlacement(3, 4, List(King))

    boards.length should be (rows * columns)
  }

  it should "contain one piece on each board when given a single piece kind" in {

    val boards = Board.findSafePlacement(3, 4, List(King))

    boards.forall(_.pieces.length == 1) should be (true)
  }

  it should "only contain board's with a piece of the kind specified when " ++
    "one is given" in {

    val kind = King

    val boards = Board.findSafePlacement(3, 4, List(kind))

    boards.forall(_.pieces forall { _.kind == kind }) should be (true)
  }

  it should "contain as many pieces in every board as many kinds were given" in {

    val kinds = List(King, King, Rook)

    val boards = Board.findSafePlacement(3, 4, kinds)

    boards.isEmpty should be (false)
    boards.forall(_.pieces.length == kinds.length) should be (true)
  }

  it should "not contain duplicate boards" in {

    val boards = Board.findSafePlacement(3, 4, List(King, King, Rook))

    boards.toSeq.groupBy(_.toString).forall(_._2.length == 1) should be (true)
  }

  it should "not place two piece's in one position on any of the boards" in {

    val boards = Board.findSafePlacement(4, 4,
      List(Rook, Rook, Knight, Knight, Knight, Knight))

    boards.forall(
      _.pieces.groupBy(_.position).forall(_._2.length == 1)
    ) should be (true)
  }
}