package it.scalachess.core.logic.moves.generators

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ FullMove, ValidMove }
import it.scalachess.core.logic.moves.generators.PieceMoves.{
  BishopMoves,
  KingMoves,
  KnightMoves,
  PawnMoves,
  QueenMoves,
  RookMoves
}
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, Queen, Rook }

object PieceWithMoveGenerator {
  implicit class PieceMoveGeneretorExt(piece: Piece) {
    def validMoves(from: Position, board: Board, history: Seq[FullMove]): Seq[ValidMove] = piece.pieceType match {
      case Pawn =>
        pieceSimpleValidMoves(from, board) ++ EnPassant(piece.color, board, from, history)
      case King =>
        pieceSimpleValidMoves(from, board) ++ Castling(piece.color, board, from, history)
      case _ => pieceSimpleValidMoves(from, board)
    }
    def pieceSimpleValidMoves(from: Position, board: Board): Seq[ValidMove] = piece.pieceType match {
      case Knight => KnightMoves(piece.color, board, from)
      case Pawn   => PawnMoves(piece.color, board, from)
      case Rook   => RookMoves(piece.color, board, from)
      case Bishop => BishopMoves(piece.color, board, from)
      case Queen  => QueenMoves(piece.color, board, from)
      case King   => KingMoves(piece.color, board, from)
    }
  }
}
