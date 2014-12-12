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

  private val allPositions =
    0 until rows flatMap (row =>
      0 until columns map (column =>
        Position(row, column)))

  val safePositions =
    allPositions filter (position =>
      pieces.forall(
        ! _.canMoveTo(position)))

  /**
   * Returns an iterable that contains all positions at which a piece of the
   * given kind can be placed without danger to it and those already on board.
   *
   * @param kind a kind of piece
   * @return all the positions at which the piece of this kind can be placed
   *         safely
   */
  def safePlacesFor(kind: PieceKind): Seq[Position] =
    safePositions
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