package org.marcjan.karol.chess_standoff

object App extends scala.App {

  val boards = Board.findSafePlacement(3, 3,
    List(King, King, Rook))

  boards foreach (println)
}
