package org.marcjan.karol.chess_standoff

import scala.collection.immutable.StreamIterator

/**
 * A board search knows how to produce all board configurations for a given
 * board size and sequence of piece kinds.
 *
 * @param rows the number of rows in the result boards
 * @param columns the number of columns in the result boards
 * @param pieceKinds kinds of pieces to place on the board
 */
private class BoardSearch(rows: Int, columns: Int, pieceKinds: List[PieceKind] = Nil) {

  /**
   * Returns the position that should be considered for adding a piece after pos
   * is.
   *
   * @param pos position we are considering to add a piece at
   * @return next position to consider adding a piece at
   */
  def incrPos(pos: Position) = {

    if (pos.row == rows)
      pos

    else if (pos.column + 1 == columns)
      Position(pos.row + 1, 0)

    else
      Position(pos.row, pos.column + 1)
  }

  import BoardSearch._

  /**
   * Returns a map whose keys are boards produced by making all valid guesses
   * about the content of the guessing position. The values are piece kind count
   * maps adjusted after making each guess.
   *
   * @param guess a tuple containing a board, position to guess at and piece
   *              kind count map
   */
  def makeGuesses(guess: Guess): Map[Board, KindCounts] = {
    val (board, pos, kindCounts) = guess

    kindCounts.flatMap(entry => {
      val (kind, count) = entry

      if (count == 0 || board.safeAt(pos, kind).unary_!
        || pos.row < 0 || pos.row >= rows
        || pos.column < 0 || pos.column >= columns)

        Nil

      else
        List((
          Board(rows, columns, kind(pos) +: board.pieces),
          decrKind(kindCounts, kind)))
    })
  }

  /**
   * Find all the guesses and finished boards that result from applying
   * makeGuesses to it's argument.
   *
   * @param guess a tuple containing a board, position to guess at and piece
   *              kind count map
   * @return list of all boards and guesses to use
   */
  private def needWorkAndDone(guess: Guess): (List[Guess], List[Board]) = {

    val (board, pos, kindCounts) = guess
    val nextPos = incrPos(pos)

    val expanded = makeGuesses(guess)

    val (rawDone, rawGuesses) = expanded partition {
      _._1.pieces.length == pieceKinds.length
    }

    val needsWork =
      if (nextPos != Position(rows, 0)) {

        rawGuesses.map({
          case (board, kindCounts) => (board, nextPos, kindCounts)

        }).toList ++ List((board, nextPos, kindCounts))

      } else {
        Nil
      }

    (needsWork, rawDone.keys.toList)
  }

  /**
   * Given a stream of guesses produces a stream of valid boards that can be
   * produced by guessing further.
   *
   * @param guesses stream of guesses
   * @return stream of valid boards
   */
  private def boardStream(guesses: Stream[Guess]): Stream[Board] = {

    guesses flatMap {
      case guess => {
        val (needWork, done) = needWorkAndDone(guess)

        done.toStream ++ boardStream(needWork.toStream)
      }
    }
  }

  /**
   * Returns an iterator over the valid boards that satisfy this search's
   * conditions.
   *
   * @return iterator producing boards
   */
  def findAll(): Iterator[Board] =

    if (pieceKinds.isEmpty) {

      Iterator(Board(rows, columns))

    } else {

      val emptyBoard = Board(0, 0)
      val startPosition = Position(0, 0)
      val kindCounts = pieceKinds.groupBy(x => x).mapValues(_.length)

      new StreamIterator(boardStream(
        (emptyBoard, startPosition, kindCounts) #:: Stream.empty))
    }
}

private object BoardSearch {

  type KindCounts = Map[PieceKind, Int]
  type Guess = (Board, Position, KindCounts)

  /**
   * Decrement the count for the given piece kind in the kind counts map.
   *
   * @param kindCounts map of kind counts
   * @param kind kind to decrement count for
   * @return map with decremented kind
   */
  def decrKind(kindCounts: KindCounts, kind: PieceKind) = {
    kindCounts.getOrElse(kind, 0) match {
      case 0 => kindCounts
      case n => kindCounts - kind + ((kind, n - 1))
    }
  }
}