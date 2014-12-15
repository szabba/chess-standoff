package org.marcjan.karol.chess_standoff

class BoardSearchSpec extends UnitSpec {

  "A BoardSearch" should "not increment a Position's column beyond it's " ++
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

  it should "not insert a new PieceKind when trying to decrement it in " ++
    "a KindCounts" in {

    val rows = 4
    val columns = 7

    val search = new BoardSearch(rows, columns)

    val kinds = Map((King, 4), (Queen, 2))

    val decredKinds = search.decrKind(kinds, Rook)

    decredKinds.get(Rook) should be (None)
  }
}
