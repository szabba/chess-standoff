package org.marcjan.karol.chess_standoff

class BoardSearchSpec extends UnitSpec {

  "A BoardSearch" should "not increment a Position's column beyond it's " +
    "column count" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    search.incrPos(Position(0, columns - 1)).column < columns should be (true)
  }

  it should "increment the row value when the column is the last valid one" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    search.incrPos(Position(0, columns - 1)).row == 1 should be (true)
  }

  it should "set column to zero when a row ends" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    search.incrPos(Position(0, columns - 1)).column == 0 should be (true)
  }

  it should "decrement the number of a present PieceKind in a KindCounts" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    val kinds = Map((King, 4), (Queen, 2))

    val decredKinds = search.decrKind(kinds, King)

    decredKinds.get(King).get should be (3)
  }

  it should "not insert a new PieceKind when trying to decrement it in " +
    "a KindCounts" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    val kinds = Map((King, 4), (Queen, 2))

    val decredKinds = search.decrKind(kinds, Rook)

    decredKinds.get(Rook) should be (None)
  }

  it should "make as many guesses as there are piece kinds with a positive " +
    "count when starting with an empty board" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    val kindCounts = Map((King, 2), (Queen, 3), (Rook, 2), (Bishop, 0))

    val guessCountExpected = kindCounts.filter(_._2 > 0).toSeq.length

    val guesses = search.makeGuesses((
      Board(rows, columns),
      Position(2, 3),
      kindCounts
      ))

    guesses.keys.toSeq.length == guessCountExpected should be (true)
  }

  it should "put a piece at the right position when making guesses" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    val position = Position(2, 3)

    val guesses = search.makeGuesses((
      Board(rows, columns),
      position,
      Map((King, 2), (Queen, 3), (Rook, 2), (Bishop, 0))
      ))

    guesses.keys.forall {
      _.pieces.exists { _.position == position }
    } should be (true)
  }

  it should "make no guesses where the guess position isn't safe" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    val guesses = search.makeGuesses((
      Board(rows, columns, List(Rook(Position(2, 1)))),
      Position(2, 3),
      Map((King, 2), (Queen, 3), (Rook, 2), (Bishop, 0))
      ))

    guesses.keys.toSeq.length should be (0)
  }

  it should "make no guesses when there are no pieces left to place" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    val guesses = search.makeGuesses((
      Board(rows, columns),
      Position(2, 3),
      Map((King, 0), (Queen, 0), (Rook, 0), (Bishop, 0))
      ))

    guesses.keys.toSeq.length should be (0)
  }

  it should "make no guesses when the guess position is outside the board" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    val guesses = search.makeGuesses((
      Board(rows, columns),
      Position(7, 3),
      Map.empty
      ))

    guesses.keys.toSeq.length should be (0)

  }
}
