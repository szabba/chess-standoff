package org.marcjan.karol.chess_standoff

import org.scalatest.prop.TableDrivenPropertyChecks

class SymmetrySearchSpec extends UnitSpec with TableDrivenPropertyChecks {

  "SymmetrySearch.findAll" should "contain only empty boards when given no " ++
    "pieces" in {

    val boards = new SymmetrySearch(3, 4).findAll

    boards.forall(_.pieces.isEmpty) should be (true)
  }

  it should "contain only one board when given no pieces" in {

    val boards = new SymmetrySearch(3, 4).findAll()

    boards.length should be (1)
  }

  it should "contain as many boards as there are positions on each board " ++
    "when given a single piece kind" in {

    val rows = 3
    val columns = 4

    val boards = new SymmetrySearch(3, 4, List(King)).findAll

    boards.length should be (rows * columns)
  }

  it should "contain one piece on each board when given a single piece kind" in {

    val boards = new SymmetrySearch(3, 4, List(King)).findAll

    boards.forall(_.pieces.length == 1) should be (true)
  }

  it should "only contain board's with a piece of the kind specified when " ++
    "one is given" in {

    val kind = King

    val boards = new SymmetrySearch(3, 4, List(kind)).findAll

    boards.forall(_.pieces forall { _.kind == kind }) should be (true)
  }

  it should "contain as many pieces in every board as many kinds were given" in {

    val pieceCountCases =
      Table(
        ("rows", "columns", "pieceKinds", "pieceCount"),
        (3, 4, List(King, King, Rook), 3),
        (3, 3, List(Rook, Knight, Bishop), 3)
      )


    forAll(pieceCountCases) {
      (rows: Int, columns: Int, pieceKinds: List[PieceKind], pieceCount: Int) => {

        val boards = new SymmetrySearch(rows, columns, pieceKinds).findAll()

        boards foreach {
          _.pieces.length should be (pieceCount)
        }
      }
    }
  }

  it should "not contain duplicate boards" in {

    val boards = new SymmetrySearch(3, 4, List(King, King, Rook)).findAll

    boards.toSeq.groupBy(_.toString).forall(_._2.length == 1) should be (true)
  }

  it should "not place two piece's in one position on any of the boards" in {

    val boards = new SymmetrySearch(4, 4,
      List(Rook, Rook, Knight, Knight, Knight, Knight)).findAll

    boards.forall(
      _.pieces.groupBy(_.position).forall(_._2.length == 1)
    ) should be (true)
  }
}
