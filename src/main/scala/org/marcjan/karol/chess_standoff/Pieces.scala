package org.marcjan.karol.chess_standoff

import scala.collection.mutable.HashMap

/**
 * A chess piece.
 */
class Piece(val kind: PieceKind, val position: Position) {

  override def toString: String =
    "Piece(%s, %s)".format(kind, position)

  /**
    * Returns true if the Piece could be moved to the specified position.
   *
   * @param position position to check with
   * @return could the piece be moved to the specified position?
   */
  def canMoveTo(position: Position): Boolean =
    kind.canMoveBy(position - this.position)
}

/**
 * A cache of Pieces.
 */
private object PieceCache {

  private val cache = new HashMap[PieceKind, HashMap[Position, Piece]]()

  /**
   * Returns a piece allocating a new one only when it's the first one with the
   * given kind an position.
   *
   * @param kind kind of the piece
   * @param position position of the piece
   * @return a piece
   */
  def get(kind: PieceKind, position: Position): Piece =
    cache.getOrElseUpdate(
      kind, new HashMap[Position, Piece]()
    ).getOrElseUpdate(
      position, new Piece(kind, position))
}

/**
 * A kind of chess piece.
 */
sealed trait PieceKind {

  /**
   * Returns true if the piece kind can move by the given displacement and false
   * otherwise. The result for a zero displacement is left unspecified.
   *
   * @param move the displacement as a tuple of (xDelta, yDelta)
   * @return can the object be moved by the given displacement?
   */
  def canMoveBy(move: Move): Boolean

  /**
   * Shortcut for creating a piece of this kind.
   *
   * @param position position to place the piece at
   * @return a piece
   */
  def apply(position: Position) =
    PieceCache.get(this, position)

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
  override def canMoveBy(move: Move): Boolean = {
    move.yDelta == 0 ||
      move.xDelta == 0 ||
      math.abs(move.xDelta) == math.abs(move.yDelta)

  }
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
    math.abs(move.xDelta) == math.abs(move.yDelta)
}

/**
 * A Knight moves by two squares in one of the non-diagonal directions and then
 * by two squares along one edge of the board and one along the other, tracing
 * the shape of the letter 'L'.
 */
object Knight extends PieceKind {
  override def canMoveBy(move: Move): Boolean = {
    val componentLengths = List(move.xDelta, move.yDelta) map math.abs

    componentLengths match {
      case List(1, 2) => true
      case List(2, 1) => true
      case _ => false
    }
  }
}

/**
 * A PieceKind that cannot move. Only for testing.
 */
private object CanMoveNowhere extends PieceKind {
  override def canMoveBy(move: Move): Boolean = false
}