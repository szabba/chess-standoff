package org.marcjan.karol.chess_standoff

import org.scalatest.{Matchers, FlatSpec}

/**
 * Spec for chessboard moves.
 */
class MoveSpec extends UnitSpec {

  "A Move" should "have the given xDelta" in {
    val xDelta = 7
    val yDelta = 3

    val move = new Move(xDelta, yDelta)

    move.xDelta should be (xDelta)
  }

  it should "have the given yDelta" in {
    val xDelta = 7
    val yDelta = 3

    val move = new Move(xDelta, yDelta)

    move.yDelta should be (yDelta)
  }

  it should "have the same xDelta the same whether created with or without new" in {
    val xDelta = 7
    val yDelta = -3

    val oneMove = new Move(xDelta, yDelta)
    val otherMove = Move(xDelta, yDelta)

    otherMove.xDelta should be (oneMove.xDelta)
  }
  
  it should "have the same yDelta the same whether created with or without new" in {
    val xDelta = 7
    val yDelta = -3

    val oneMove = new Move(xDelta, yDelta)
    val otherMove = Move(xDelta, yDelta)

    otherMove.yDelta should be (oneMove.yDelta)
  }

  import Move._

  "isDiagonal" should "hold for Move(1, 1)" in {
    Move(1, 1).isDiagonal should be (true)
  }

   it should "hold for Move(2, 2)" in {
    Move(2, 2).isDiagonal should be (true)
  }

  it should "hold for Move(-1, -1)" in {
    Move(-1, -1).isDiagonal should be (true)
  }

  it should "not hold for Move(2, 3)" in {
    Move(2, 3).isDiagonal should be (false)
  }

  it should "not hold for Move(-3, 2)" in {
    Move(-3, 2).isDiagonal should be (false)
  }

  "isAlongBoardEdge" should "hold for Move(0, 1)" in {
    Move(0, 1).isAlongBoardEdge should be (true)
  }

  it should "hold for Move(0, 2)" in {
    Move(0, 2).isAlongBoardEdge should be (true)
  }

  it should "hold for Move(1, 0)" in {
    Move(1, 0).isAlongBoardEdge should be (true)
  }

  it should "hold for Move(0, -1)" in {
    Move(0, -1).isAlongBoardEdge should be (true)
  }

  it should "hold for Move(-1, 0)" in {
    Move(-1, 0).isAlongBoardEdge should be (true)
  }

  it should "not hold for Move(1, 2)" in {
    Move(1, 2).isAlongBoardEdge should be (false)
  }

  it should "not hold for Move(1, 1)" in {
    Move(1, 1).isAlongBoardEdge should be (false)
  }

  it should "not hold for Move(-1, 1)" in {
    Move(-1, 1).isAlongBoardEdge should be (false)
  }

  "isZero" should "not hold for Move(0, 1)" in {
    Move(0, 1).isZero should be (false)
  }

  it should "not hold for Move(1, 0)" in {
    Move(1, 0).isZero should be (false)
  }

  it should "not hold for Move(-1, 0)" in {
    Move(-1, 0).isZero should be (false)
  }

  it should "not hold for Move(2, 3)" in {
    Move(2, 3).isZero should be (false)
  }

  it should "hold for Move(0, 0)" in {
    Move(0, 0).isZero should be (true)
  }

  "movesExcept" should "return Nil when called with two identical arguments" in {
    val someMoves = List((2, 3), (-1, 4), (0, 7), (3, -1))

    movesExcept(someMoves, someMoves) should be (Nil)
  }

  it should "return Nil when the first argument is empty" in {
    val someMoves = List((2, 3), (-1, 4), (0, 7), (3, -1))

    movesExcept(Nil, someMoves) should be (Nil)
  }

  it should "filter nothing when the second argument is Nil" in {
    val someMoves = List((2, 3), (-1, 4), (0, 7), (3, -1))

    val allMoves = movesExcept(someMoves, Nil)

    allMoves contains (2, 3) should be (true)
    allMoves contains (-1, 4) should be (true)
    allMoves contains (0, 7) should be (true)
    allMoves contains (3, -1) should be (true)
  }

  it should "filter nothing when the arguments have no common elements" in {
    val someMoves = List((2, 3), (-1, 4), (0, 7), (3, -1))
    val someOtherMoves = List((3, 2), (7, 0), (8, 6))

    val allMoves = movesExcept(someMoves, someOtherMoves)

    allMoves contains (2, 3) should be (true)
    allMoves contains (-1, 4) should be (true)
    allMoves contains (0, 7) should be (true)
    allMoves contains (3, -1) should be (true)
  }

  it should "keep elements of the first argument not in the second" in {
    val someMoves = List((2, 3), (-1, 4), (0, 7), (3, -1))
    val movesToExclude = List((-1, 4), (3, -1))

    val filtered = movesExcept(someMoves, movesToExclude)

    filtered contains (2, 3) should be (true)
    filtered contains (0, 7) should be (true)
  }

  it should "not keep element of the first argument present in the second" in {
     val someMoves = List((2, 3), (-1, 4), (0, 7), (3, -1))
     val movesToExclude = List((-1, 4), (3, -1))

     val filtered = movesExcept(someMoves, movesToExclude)

     filtered contains (-1, 4) should be (false)
     filtered contains (3, -1) should be (false)
  }
}
