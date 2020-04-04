package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.generators.PieceGenerators.PieceWithMoveGenerator
import it.scalachess.core.logic.moves.{ FullMove, ValidMove }
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, Queen, Rook }

class MoveGenerator(board: Board, player: Color) {

  /**
   * All valid moves, except the one that leaves the king in check.
   * @return a list of full moves.
   */
  def allMoves(): List[FullMove] =
    validMovesWithoutCheck()
      .map(move => FullMove(move, resultsInCheck(move, player.other), resultsInCheckmate(move, player.other)))
      .filter(move => !resultsInCheck(move.validMove, player))

  def validMovesWithoutCheck(): List[ValidMove] =
    board.pieces
      .filter(_._2.color == player)
      .flatMap { case (pos, piece) => piece.validMoves(pos, board) }
      .toList

  def resultsInCheck(move: ValidMove, kingColor: Color): Boolean = {
    val afterBoard = board.apply(move.boardChanges)
    new MoveGenerator(afterBoard, kingColor.other)
      .validMovesWithoutCheck()
      .exists(capturesKing(_, kingColor, afterBoard))
  }

  def resultsInCheckmate(move: ValidMove, kingColor: Color): Boolean = {
    val afterBoard = board.apply(move.boardChanges)
    val result = new MoveGenerator(afterBoard, kingColor)
      .validMovesWithoutCheck()
      .filter(resultsInCheck(_, kingColor))
    result.isEmpty
  }

  private def capturesKing(move: ValidMove, kingColor: Color, board: Board): Boolean =
    board
      .apply(move.boardChanges)
      .kingPos(kingColor)
      .isEmpty
}

object PieceGenerators {
  implicit class PieceWithMoveGenerator(piece: Piece) {
    def validMoves(from: Position, board: Board): List[ValidMove] = piece.pieceType match {
      case Knight => GenerateKnightMoves(piece.color, board, from)
      case Pawn =>
        GeneratePawnMoves(piece.color, board, from) // ::: GeneratePawnSpecialMoves(pieceType, player, board, from)
      case Rook   => GenerateRookMoves(piece.color, board, from)
      case Bishop => GenerateBishopMoves(piece.color, board, from)
      case Queen  => GenerateQueenMoves(piece.color, board, from)
      case King   => GenerateKingMoves(piece.color, board, from)
    }
  }
}
