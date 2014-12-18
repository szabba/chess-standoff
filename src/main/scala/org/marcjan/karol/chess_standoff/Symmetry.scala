package org.marcjan.karol.chess_standoff

/**
 * A Symmetry can transform between "morally equivalent" boards.
 */
private sealed trait Symmetry {

  /**
   * Returns the board transformed by the symmetry.
   *
   * @param board board to transform
   * @return board transformed by the symmetry
   */
  def apply(board: Board): Board = {

    val pieces = board.pieces map {
      case piece => {
        piece.kind(
          apply(board.rows, board.columns, piece.position))
      }
    }

    new Board(board.rows, board.columns, pieces)
  }

  /**
   * Returns a position transformed through the symmetry assuming the given
   * board dimensions.
   *
   * @param rows number of board rows
   * @param columns number of board columns
   * @param position position to transform
   * @return transformed position
   */
  def apply(rows: Int, columns: Int, position: Position): Position
}

/**
 * A symmetry that transforms a board into itself.
 */
private object Identity extends Symmetry {
  /**
   * Returns a position transformed through the symmetry assuming the given
   * board dimensions.
   *
   * @param rows number of board rows
   * @param columns number of board columns
   * @param position position to transform
   * @return transformed position
   */
  override def apply(rows: Int, columns: Int, position: Position): Position = position
}

/**
 * A symmetry that mirrors each row of the board.
 */
private object MirrorRows extends Symmetry {
  /**
   * Returns a position transformed through the symmetry assuming the given
   * board dimensions.
   *
   * @param rows number of board rows
   * @param columns number of board columns
   * @param position position to transform
   * @return transformed position
   */
  override def apply(rows: Int, columns: Int, position: Position): Position =
    Position(position.row, columns - position.column - 1)
}