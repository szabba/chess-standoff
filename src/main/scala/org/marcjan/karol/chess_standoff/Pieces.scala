package org.marcjan.karol.chess_standoff

/**
 * A chess piece.
 */
sealed abstract class Piece {
  def canMoveBy(position: (Int, Int)): Boolean
}

/**
 * A King piece. It can move in any of the eight directions but only by one
 * square.
 */
object King extends Piece {
  override def canMoveBy(positionDelta: (Int, Int)): Boolean =
    positionDelta match {
      case (1, 0) => true
      case _ => false
    }
}

/**
 * A Queen piece. It can move along a straight line in any of the eight
 * directions by any number of squares.
 */
object Queen extends Piece {
  override def canMoveBy(positionDelta: (Int, Int)): Boolean = false
}

/**
 * A Rook piece. It can move along a straight line in the four non-diagonal
 * directions by any number of squares.
 */
object Rook extends Piece {
  override def canMoveBy(positionDelta: (Int, Int)): Boolean = false
}

/**
 * A Bishop piece. It can move along a straight line in the four diagonal
 * directions by any number of squares.
 */
object Bishop extends Piece {
  override def canMoveBy(positionDelta: (Int, Int)): Boolean = false
}

/**
 * A Knight piece. It moves by two squares in one of the non-diagonal
 * directions and then by two squares in a direction perpendicular to the first
 * one.
 */
object Knight extends Piece {
  override def canMoveBy(positionDelta: (Int, Int)): Boolean = false
}