package org.marcjan.karol.chess_standoff

import java.util.Scanner

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