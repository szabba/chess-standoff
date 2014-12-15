package org.marcjan.karol.chess_standoff


private class BoardSearch(rows: Int, columns: Int, pieces: List[PieceKind]=Nil) {

  private[chess_standoff] type KindCount = Map[PieceKind, Int]
  private[chess_standoff] type Guess = (Board, Position, KindCount)

  private[chess_standoff] def incrPos(pos: Position) = {

    if (pos.row == rows) pos

    else if (pos.column + 1 == columns)
      Position(pos.row + 1, 0)

    else Position(pos.row, pos.column + 1)
  }
}
