package org.marcjan.karol.chess_standoff

/**
 * A chessboard with possibly non-standard size.
 *
 * @param rows number of rows the board should have
 * @param columns number of columns the board should have
 * @param piecesGiven a sequence of chess pieces you wish to place on the board
 */
class Board(val rows: Int, val columns: Int, piecesGiven: Seq[Piece]=List()) {

  override def toString: String =
    pieces.map(_.toString).sorted.mkString(", ")

  val pieces: Seq[Piece] = piecesGiven

  private val allPositions =
    0 until rows flatMap (row =>
      0 until columns map (column =>
        Position(row, column)))

  private val safePositions =
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
  private[chess_standoff] def safePlacesFor(kind: PieceKind): Seq[Position] =
    safePositions filterNot (candidatePosition =>
      pieces exists (piece =>
        kind(candidatePosition).canMoveTo(piece.position)))

  /**
   * Creates all board that result from adding a piece of the given kind
   * somewhere safe.
   *
   * @param kind kind of the piece to add
   * @return possibly empty iterable of boards
   */
  def placeWithoutConflict(kind: PieceKind): Iterable[Board] =
    safePlacesFor(kind) map (position =>
      Board(rows, columns, pieces :+ kind(position)))
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
  def apply(rows: Int, columns: Int, pieces: Seq[Piece]=List()): Board =
    new Board(rows, columns, pieces)

  def findSafePlacement(rows: Int, columns: Int, pieces: Seq[PieceKind]=List()):
    Seq[Board] = {

    if (pieces.isEmpty)
      List(Board(rows, columns))
    else
      1 to (rows * columns) map (_ => Board(rows, columns))
  }
}