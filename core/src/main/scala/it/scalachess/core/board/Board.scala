package it.scalachess.core.board

import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, PieceType, Queen, Rook }

import it.scalachess.core.colors.{ Black, White }
case class Board(
    pieces: Map[Position, Piece]
) {

  def pieceAt(pos: Position): Option[Piece] = pieces get pos
}

object Board {
  val width: Int  = 8
  val height: Int = 8

  def isInside(x: Int, y: Int): Boolean =
    x >= 1 && x <= width && y >= 1 && y <= height

  def defaultBoard(): Board = {
    val pieceMap = {
      for (i <- Seq(1, 2, height - 1, height);
           j <- 1 to 8) yield {
        Position.of(i, j)
          .map({ pos =>
            val color = if (i <= 2) White else Black
            val piece = Piece(color, initialPieceTypeAtPosition(pos))
            (pos, piece)
          })
      }
    }.flatten.toMap

    Board(pieceMap)
  }

  private def initialPieceTypeAtPosition(pos: Position): PieceType =
    pos.x match {
      case 1 | 8 =>
        pos.y match {
          case 1 | 8 => Rook
          case 2 | 7 => Knight
          case 3 | 6 => Bishop
          case 4     => King
          case 5     => Queen
        }
      case 2 | 7 => Pawn
    }

  def fromFEN(fen: String): Board =
    Board(Map[Position, Piece]())
}
