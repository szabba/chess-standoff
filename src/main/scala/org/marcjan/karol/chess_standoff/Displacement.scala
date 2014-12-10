package org.marcjan.karol.chess_standoff

object Displacement {

  /**
   * Returns true when the given displacement is diagonal and false otherwise.
   * It is left unspecified for the zero displacement.
   *
   * @param displacement a displacement as a tuple of (xDelta, yDelta)
   * @return is the displacement diagonal?
   */
  def isDiagonal(displacement: (Int, Int)): Boolean =
    displacement match {
      case (xDelta, yDelta) => xDelta == yDelta
    }

  /**
   * Returns true when the given displacement is along the board's edge. It is
   * left unspecified for the zero displacement.
   *
   * @param displacement a displacement as a tuple of (xDelta, yDelta)
   * @return is the displacement along the board's edge?
   */
  def isAlongBoardEdge(displacement: (Int, Int)): Boolean = false
}