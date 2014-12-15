package org.marcjan.karol.chess_standoff


private class BoardSearch(rows: Int, columns: Int, pieces: List[PieceKind]=Nil) {

  private[chess_standoff] type KindCounts = Map[PieceKind, Int]
  private[chess_standoff] type Guess = (Board, Position, KindCounts)

  private[chess_standoff] def incrPos(pos: Position) = {

    if (pos.row == rows) pos

    else if (pos.column + 1 == columns)
      Position(pos.row + 1, 0)

    else Position(pos.row, pos.column + 1)
  }

  private[chess_standoff] def decrKind(kindCounts: KindCounts, kind: PieceKind) = {
    kindCounts.getOrElse(kind, 0) match {
      case 0 => kindCounts
      case n => kindCounts - kind + ((kind, n - 1))
    }
  }
}
