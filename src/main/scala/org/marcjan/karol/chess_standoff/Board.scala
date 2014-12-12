package org.marcjan.karol.chess_standoff

import scala.collection.LinearSeq

/**
 * A chessboard with possibly non-standard size.
 */
class Board(val rows: Int, val columns: Int, piecesGiven: LinearSeq[Piece]=List()) {
  val pieces = piecesGiven
}
