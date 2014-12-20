package org.marcjan.karol.chess_standoff

import scala.annotation.tailrec

private class SymmetrySearch(rows: Int, columns: Int, pieceKinds: List[PieceKind] = Nil) {

  def findAll(): Iterator[Board] = {

    val emptyBoard = new Board(rows, columns)

    if (pieceKinds.isEmpty) {
      Iterator(emptyBoard)

    } else {

      val counts = pieceKinds.groupBy(x => x).mapValues(_.length)
      val initialGuess = (Set(emptyBoard), counts)

      loop(Set(initialGuess)).toIterator
    }
  }

  import SymmetrySearch._

  private var symmetries: Set[Symmetry] = symmetryGroup(rows, columns)

  @tailrec
  private def loop(guesses: Set[Guess], results: Set[Board] = Set.empty): Set[Board] = {

    if (guesses.isEmpty) {
      results
    } else {
      val (needWork, done) = needWorkAndDone(guesses)

      loop(needWork, done ++ results)
    }
  }

  private def needWorkAndDone(guesses: Set[(BoardClass, KindCounts)]): (Set[Guess], Set[Board]) = {
    val (needWork, doneGuessing): (Set[Guess], Set[Guess]) = guesses.flatMap {
      makeGuesses(_)
    } partition {
      case (_, counts) => counts.values.sum > 0
    }

    (needWork, doneGuessing.flatMap(_._1))
  }

  private def makeGuesses(guess: Guess): Set[Guess] = {
    val (boardClass, kindCounts) = guess

    boardClass.headOption match {
      case None => Set.empty

      case Some(board) => {
        kindCounts.keySet.flatMap(guessForKind(_, board, kindCounts))
      }
    }
  }

  def guessForKind(kind: PieceKind, board: Board, counts: KindCounts): Set[Guess] = {
    board.safeSpace flatMap {
      case position => {

        if (board.pieces exists {
          case piece => kind.canMoveBy(piece.position - position)
        }) {
          Set.empty[Guess]

        } else {
          Set((
            classFor(board, kind(position)),
            decrKind(counts, kind)))
        }

      }
    } toSet
  }

  def classFor(board: Board, piece: Piece): BoardClass = {

    val newPieces = piece +: board.pieces
    val spaceLeft = Some(
      board.safeSpace filterNot {
        case position => piece.canMoveTo(position) || piece.position == position
      }
    )

    val classSeed = new Board(rows, columns, piece +: board.pieces, spaceLeft)

    symmetries map { _(classSeed) }
  }
}

private object SymmetrySearch {

  type KindCounts = Map[PieceKind, Int]
  type BoardClass = Set[Board]
  type Guess = (BoardClass, KindCounts)

  private val rectangleSymmetries = Set(
    Identity, MirrorRows, MirrorColumns, RotateRightBy180Degrees)

  private val squareOnlySymmetries = Set(
    MirrorDiagonal, MirrorAntidiagonal,
    RotateRightBy90Degrees, RotateRightBy270Degrees)

  /**
   * Returns the list of all symmetries of boards with the given dimensions.
   *
   * @param rows number of board rows
   * @param columns number of board columns
   * @return list of symmetries that apply to boards with the given numbers of
   *         rows and dimmensions
   */
  def symmetryGroup(rows: Int, columns: Int): Set[Symmetry] = {
    if (rows != columns)
      rectangleSymmetries
    else
      rectangleSymmetries ++ squareOnlySymmetries
  }

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