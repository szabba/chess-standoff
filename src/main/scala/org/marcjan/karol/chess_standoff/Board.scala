package org.marcjan.karol.chess_standoff

/**
 * A chessboard with possibly non-standard size.
 *
 * @param rows number of rows the board should have
 * @param columns number of columns the board should have
 * @param piecesGiven a sequence of chess pieces you wish to place on the board
 */
class Board(val rows: Int, val columns: Int, piecesGiven: Seq[Piece]=List()) {

  override def toString: String = {
    var builder = new StringBuilder()
    for (i <- 0 until rows; j <- 0 until columns) {

      val pos = Position(i, j)
      val piecesAt = pieces filter {
        _.position == pos
      }

      if (piecesAt.isEmpty)
        builder ++= "_"
      else
        builder ++= piecesAt.head.kind.toString

      if (j + 1 == columns)
        builder += '\n'
    }
    builder.toString
  }

  val pieces: Seq[Piece] = piecesGiven

  private val allPositions =
    0 until rows flatMap (row =>
      0 until columns map (column =>
        Position(row, column)))

  private val safePositions =
    allPositions filter (position =>
      pieces.forall(
        ! _.canMoveTo(position))
    ) filterNot (position =>
      pieces map(_.position) exists(_ == position)
    )

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
   * Checks whether a piece of the given kind can be placed at the given
   * position on the board.
   *
   * @param position position to check at
   * @param kind kind of chess piecce to check for
   * @return can a piece of the given kind be safely placed at the given
   *         position on this board?
   */
  def safeAt(position: Position, kind: PieceKind): Boolean =
    pieces forall (piece =>
      piece.canMoveTo(position).unary_! &&
      piece.position != position)

  /**
   * Creates all boards that result from adding a piece of the given kind
   * somewhere safe.
   *
   * @param kind kind of the piece to add
   * @return possibly empty iterable of boards
   */
  def placeWithoutConflict(kind: PieceKind): Seq[Board] =
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

    def loop(boards: Seq[Board], pieces: Seq[PieceKind]): Seq[Board] =
      if (pieces.isEmpty) boards
      else loop(
        boards.flatMap(_.placeWithoutConflict(pieces.head)),
        pieces.tail)

    loop(List(Board(rows, columns)), pieces).groupBy(
      _.toString).toSeq.map(_._2.head)
  }
}