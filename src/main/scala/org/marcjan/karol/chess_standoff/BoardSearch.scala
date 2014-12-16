package org.marcjan.karol.chess_standoff

import scala.collection.immutable.StreamIterator

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

  private def boardStream(guesses: Stream[Guess]): Stream[Board] = {

    guesses flatMap {
      case guess =>
        val (needWork, done) = needsWorkAndDone(guess)

        done.toStream ++ boardStream(needWork.toStream)
    }
  }

  def findAll(): Iterator[Board] =

    if (pieces.isEmpty)
      Iterator(Board(rows, columns))

    else {

      val emptyBoard = Board(0, 0)
      val startPosition = Position(0, 0)
      val kindCounts = pieces.groupBy(x => x).mapValues(_.length)

      new StreamIterator(boardStream(
        (emptyBoard, startPosition, kindCounts) #:: Stream.empty))
    }
}