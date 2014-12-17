package org.marcjan.karol.chess_standoff

import java.util.Scanner

/**
 * Reads the board dimensions (first rows, then columns) and the number of
 * pieces of each kind (in order: (K)ings, (Q)ueens, (R)ooks, (B)ishops and
 * K(N)ights) to place on the board. Then prints out all the valid placement of
 * the given piece set on a board of the given size and states their number.
 */
object Main extends scala.App {

  val pieceKindOrder = List(King, Queen, Rook, Bishop, Knight)
  
  val scanner = new Scanner(Console.in)

  val rows = scanner.nextInt()
  val columns = scanner.nextInt()

  val pieceKinds = pieceKindOrder flatMap {
    List.fill(scanner.nextInt)(_)
  }

  val boards = Board.findSafePlacement(rows, columns, pieceKinds)

  var count = 0
  boards foreach {

    case board => {
      count = count + 1

      println(board)
    }
  }

  printf("Found %d unique boards.\n", count)
}