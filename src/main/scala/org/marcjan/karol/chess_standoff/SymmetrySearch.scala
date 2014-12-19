package org.marcjan.karol.chess_standoff

private object SymmetrySearch {

  type Nexter = Position => Position
  type KindCounts = Map[PieceKind, Int]

  private val rectangleSymmetries = Set(
    Identity, MirrorRows, MirrorColumns, RotateRightBy180Degrees)

  private val squareOnlySymmetries = Set(
    MirrorDiagonal, MirrorAntidiagonal,
    RotateRightBy90Degrees, RotateRightBy270Degrees)

  /**
   * Returns the list of all symmetries of boards with the given dimensions.
   *
   * @param rows number of board rows
   * @param columns number of board columns
   * @return list of symmetries that apply to boards with the given numbers of
   *         rows and dimmensions
   */
  def symmetryGroup(rows: Int, columns: Int): Set[Symmetry] = {
    if (rows != columns)
      rectangleSymmetries
    else
      rectangleSymmetries ++ squareOnlySymmetries
  }
}