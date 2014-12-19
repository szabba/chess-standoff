package org.marcjan.karol.chess_standoff

private object SymmetrySearch {

  type Nexter = Position => Position
  type KindCounts = Map[PieceKind, Int]
}
