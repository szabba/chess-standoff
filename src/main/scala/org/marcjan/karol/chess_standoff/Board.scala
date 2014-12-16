package org.marcjan.karol.chess_standoff

/**
 * A chessboard with possibly non-standard size.
 *
 * @param rows number of rows the board should have
 * @param columns number of columns the board should have
 * @param piecesGiven a sequence of chess pieces you wish to place on the board
 */
class Board(val rows: Int, val columns: Int, piecesGiven: Seq[Piece]=List()) {

  /**
   * Returns true when the board is valid, that is:
   * <ul>
   *   <li>all the pieces are actually on the board,</li>
   *   <li>no two pieces hold the same position,</li>
   *   <li>and no piece can take another.</li>
   * </ul>
   *
   * @return is this board valid?
   */
  def isValid(): Boolean = {
    val allInRange =
      pieces.forall(p =>
        0 <= p.position.row
        && p.position.row < rows
        && 0 <= p.position.column
        && p.position.column < columns)

    val allUnique =
      pieces.forall(p =>
        pieces.filter(_.position == p.position).length == 1)

    val noneCanTakeAnother =
      pieces.forall(p =>
        pieces.exists(r =>
          r.position != p.position && p.canMoveTo(r.position)
        ).unary_!)

    allInRange && allInRange && noneCanTakeAnother
  }

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
      piece.canMoveTo(position).unary_!
        && piece.position != position
        && kind.canMoveBy(piece.position - position).unary_!)
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

    new BoardSearch(rows, columns, pieces.toList).findAll
  }
}