package org.marcjan.karol.chess_standoff

/**
 * A chess piece.
 */
sealed class Piece(val kind: PieceKind, val position: Position) extends CanMoverBy {

  /**
   * Returns true if the object can move by the given displacement and false
   * otherwise. The result for a zero displacement is left unspecified.
   *
   * @param move the displacement as a tuple of (xDelta, yDelta)
   * @return can the object be moved by the given displacement?
   */
  override def canMoveBy(move: Move): Boolean =
    kind.canMoveBy(move)

  /**
    * Returns true if the Piece could be moved to the specified position.
   *
   * @param position position to check with
   * @return could the piece be moved to the specified position?
   */
  def canMoveTo(position: Position): Boolean =
    canMoveBy(position - this.position)
}

/**
 * A kind of chess piece.
 */
trait PieceKind extends CanMoverBy {

  /**
   * Shortcut for creating a piece of this kind.
   *
   * @param position position to place the piece at
   * @return a piece
   */
  def apply(position: Position) =
    new Piece(this, position)

  /**
   * Returns the name of the piece kind in English algebraic notation.
   *
   * @return name of the piece kind in algebraic notation
   */
  override def toString: String =
    this match {
      case King => "K"
      case Queen => "Q"
      case Rook => "R"
      case Bishop => "B"
      case Knight => "N"
    }
}

/**
 * A King can move in any of the eight directions but only by one square.
 */
object King extends PieceKind {

  override def canMoveBy(move: Move): Boolean =
    (-1 to 1 contains move.xDelta) && (-1 to 1 contains move.yDelta)
}

/**
 * A Queen can move along a straight line in any of the eight directions by any
 * number of squares.
 */
object Queen extends PieceKind {
  override def canMoveBy(move: Move): Boolean =
    move.yDelta == 0 || move.xDelta == 0 || move.xDelta == move.yDelta
}

/**
 * A Rook can move along a straight line in the four non-diagonal directions by
 * any number of squares.
 */
object Rook extends PieceKind {
  override def canMoveBy(move: Move): Boolean =
    move.xDelta == 0 || move.yDelta == 0
}

/**
 * A Bishop can move along a straight line in the four diagonal directions by
 * any number of squares.
 */
object Bishop extends PieceKind {
  override def canMoveBy(move: Move): Boolean =
    move.xDelta == move.yDelta
}

/**
 * A Knight moves by two squares in one of the non-diagonal directions and then
 * by two squares along one edge of the board and one along the other, tracing
 * the shape of the letter 'L'.
 */
object Knight extends PieceKind {
  override def canMoveBy(move: Move): Boolean = {
    val components = List(move.xDelta, move.yDelta)
    val absComponents = components map math.abs sortWith (_ < _)

    absComponents match {
      case List(1, 2) => true
      case _ => false
    }
  }
}