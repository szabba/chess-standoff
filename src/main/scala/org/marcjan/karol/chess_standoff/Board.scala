package org.marcjan.karol.chess_standoff

import scala.collection.LinearSeq

/**
 * A chessboard with possibly non-standard size.
 *
 * @param rows number of rows the board should have
 * @param columns number of columns the board should have
 * @param piecesGiven a sequence of chess pieces you wish to place on the board
 */
class Board(val rows: Int, val columns: Int, piecesGiven: LinearSeq[Piece]=List()) {

  val pieces = piecesGiven

  val safePositions = {
    0 until rows flatMap (row =>
      0 until columns map (column =>
        Position(row, column)))
  }
}

/**
 * Companion of the Board class
 */
object Board {

  /**
   * Shortcut for creating Boards without the new keyword.
   *
   * @param rows number of rows the board should have
   * @param columns number of columns the board should have
   * @param pieces a sequence of chess pieces you wish to place on the board
   * @return a board
   */
  def apply(rows: Int, columns: Int, pieces: LinearSeq[Piece]=List()): Board =
    new Board(rows, columns, pieces)
}