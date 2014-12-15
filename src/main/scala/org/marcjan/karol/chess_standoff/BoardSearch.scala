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

  private[chess_standoff] def makeGuesses(guess: Guess): Map[Board, KindCounts] = {
    val (board, pos, kindCounts) = guess

    kindCounts.flatMap(entry => {
      val (kind, count) = entry

      if (count == 0 || board.safeAt(pos, kind).unary_!
        || pos.row < 0 || pos.row >= rows
        || pos.column < 0 || pos.column >= columns)

        Nil

      else List(
        (
          Board(rows, columns,
            kind(pos) +: board.pieces),
          decrKind(kindCounts, kind)))
    })
  }

  private def needsWorkAndDone(guess: Guess): (List[Guess], List[Board]) = {

    val (board, pos, kindCounts) = guess
    val nextPos = incrPos(pos)

    val expanded = makeGuesses(guess)

    val done = expanded.map(
      _._1
    ).filter(
        _.pieces.length == pieces.length
      ).toList

    val mightNeedWork = expanded.filterNot(
      _._1.pieces.length == pieces.length
    ).map {
      case (board, kindCounts) => (board, nextPos, kindCounts)
    }

    val needsWork =

      if (nextPos != Position(rows, 0))

        mightNeedWork.toList ++
          List((board, nextPos, kindCounts))

      else Nil

    (needsWork, done)
  }

  private var found = 0

  private def loop(guesses: List[Guess], results: List[Board]=Nil): List[Board] = {

    guesses match {
      case Nil => results
      case guess :: rest =>

        val (needWork, done) = needsWorkAndDone(guess)

        loop(needWork ++ rest, done ++ results)
    }
  }

  def findAll(): List[Board] = {
    loop(List((
      Board(rows, columns),
      Position(0, 0),
      pieces.groupBy(x => x).mapValues(_.length)
      )))
  }
}
