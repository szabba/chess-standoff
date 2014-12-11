package org.marcjan.karol.chess_standoff

import org.scalatest.{Matchers, FlatSpec}

/**
 * Spec for the functions in the Move object.
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

  "isDiagonal" should "hold for (1, 1)" in {
    isDiagonal((1, 1)) should be (true)
  }

   it should "hold for (2, 2)" in {
    isDiagonal((2, 2)) should be (true)
  }

  it should "hold for (-1, -1)" in {
    isDiagonal((-1, -1)) should be (true)
  }

  it should "not hold for (2, 3)" in {
    isDiagonal((2, 3)) should be (false)
  }

  it should "not hold for (-3, 2)" in {
    isDiagonal((-3, 2)) should be (false)
  }

  "isAlongBoardEdge" should "hold for (0, 1)" in {
    isAlongBoardEdge((0, 1)) should be (true)
  }

  it should "hold for (0, 2)" in {
    isAlongBoardEdge((0, 2)) should be (true)
  }

  it should "hold for (1, 0)" in {
    isAlongBoardEdge((1, 0)) should be (true)
  }

  it should "hold for (0, -1)" in {
    isAlongBoardEdge((0, -1)) should be (true)
  }

  it should "hold for (-1, 0)" in {
    isAlongBoardEdge((-1, 0)) should be (true)
  }

  it should "not hold for (1, 2)" in {
    isAlongBoardEdge((1, 2)) should be (false)
  }

  it should "not hold for (1, 1)" in {
    isAlongBoardEdge((1, 1)) should be (false)
  }

  it should "not hold for (-1, 1)" in {
    isAlongBoardEdge((-1, 1)) should be (false)
  }

  "isZero" should "not hold for (0, 1)" in {
    isZero((0, 1)) should be (false)
  }

  it should "not hold for (1, 0)" in {
    isZero((1, 0)) should be (false)
  }

  it should "not hold for (-1, 0)" in {
    isZero((-1, 0)) should be (false)
  }

  it should "not hold for (2, 3)" in {
    isZero((2, 3)) should be (false)
  }

  it should "hold for (0, 0)" in {
    isZero(0, 0) should be (true)
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
