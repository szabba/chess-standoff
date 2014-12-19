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

  /**
   * Symmetry inverting the effects of the containing one.
   */
  val inverse: Symmetry
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

  override val inverse: Symmetry = this
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

  override val inverse: Symmetry = this
}

/**
 * A symmetry that mirrors each column of the board.
 */
private object MirrorColumns extends Symmetry {
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
    Position(rows - position.row - 1, position.column)

  override val inverse: Symmetry = this
}

/**
 * A symmetry that rotates a board by 180 degrees.
 */
private object RotateRightBy180Degrees extends Symmetry {
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
    Position(rows - position.row - 1, columns - position.column - 1)

  override val inverse: Symmetry = this
}

/**
 * A symmetry that mirrors a board with respect to it's main diagonal. The
 * results are undefined for non-square boards.
 */
private object MirrorDiagonal extends Symmetry {
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
    Position(position.column, position.row)

  override val inverse: Symmetry = this
}

/**
 * A symmetry that mirros a board with respect to it's antidiagonal. The results
 * are undefined for a non-square board.
 */
private object MirrorAntidiagonal extends Symmetry {
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
    Position(columns - position.column - 1, rows - position.row - 1)

  override val inverse: Symmetry = this
}

/**
 * A symmetry that rotates the board by 90 degrees to the right. The results are
 * undefined for a non-square board.
 */
private object RotateRightBy90Degrees extends Symmetry {
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
    Position(columns - position.column - 1, position.row)

  override val inverse: Symmetry = RotateRightBy270Degrees
}

/**
 * A symmetry that rotates the board by 270 degrees to the right. The results
 * are undefined for a non-square board.
 */
private object RotateRightBy270Degrees extends Symmetry {
  /**
   * Returns a position transformed through the symmetry assuming the given
   * board dimensions.
   *
   * @param rows number of board rows
   * @param columns number of board columns
   * @param position position to transform
   * @return transformed position
   */
  override def apply(rows: Int, columns: Int, position: Position): Position = {
    RotateRightBy90Degrees(rows, columns,
      RotateRightBy90Degrees(rows, columns,
        RotateRightBy90Degrees(rows, columns, position)))
  }

  override val inverse: Symmetry = RotateRightBy90Degrees
}