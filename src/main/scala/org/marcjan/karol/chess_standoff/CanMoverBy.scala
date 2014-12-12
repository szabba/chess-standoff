package org.marcjan.karol.chess_standoff

/**
 * A trait for objects that know if they can be moved relatively.
 */
trait CanMoverBy {

  /**
   * Returns true if the object can move by the given displacement and false
   * otherwise. The result for a zero displacement is left unspecified.
   *
   * @param move the displacement as a tuple of (xDelta, yDelta)
   * @return can the object be moved by the given displacement?
   */
  def canMoveBy(move: Move): Boolean
}
