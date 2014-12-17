package org.marcjan.karol.chess_standoff

import java.util.Scanner

object Main extends scala.App {

  val scanner = new Scanner(Console.in)

  val rows = scanner.nextInt
  val columns = scanner.nextInt

  val pieces = Piece.kinds flatMap {
    List.fill(scanner.nextInt)(_)
  }

  val boards = Board.findSafePlacement(
    rows, columns, pieces)

  var count = 0
  boards foreach {

    case board =>
      count = count + 1

      println(board)
  }

  printf("Found %d unique boards.\n", count)
}