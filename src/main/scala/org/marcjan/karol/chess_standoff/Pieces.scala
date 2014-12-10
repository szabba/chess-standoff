package org.marcjan.karol.chess_standoff

/**
 * A chess piece.
 */
sealed abstract class Piece

/**
 * A King piece. It can move in any of the eight directions but only by one
 * square.
 */
object King extends Piece

/**
 * A Queen piece. It can move along a straight line in any of the eight
 * directions by any number of squares.
 */
object Queen extends Piece

/**
 * A Rook piece. It can move along a straight line in the four non-diagonal
 * directions by any number of squares.
 */
object Rook extends Piece

/**
 * A Bishop piece. It can move along a straight line in the four diagonal
 * directions by any number of squares.
 */
object Bishop extends Piece

/**
 * A Knight piece. It moves by two squares in one of the non-diagonal
 * directions and then by two squares in a direction perpendicular to the first
 * one.
 */
object Knight extends Piece