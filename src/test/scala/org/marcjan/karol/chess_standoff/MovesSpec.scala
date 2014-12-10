package org.marcjan.karol.chess_standoff

import org.scalatest.{Matchers, FlatSpec}

class MovesSpec extends FlatSpec with Matchers {

  import Moves._

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
}
