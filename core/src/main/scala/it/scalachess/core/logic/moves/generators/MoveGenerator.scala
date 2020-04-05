package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.generators.PieceGenerators.PieceWithMoveGenerator
import it.scalachess.core.logic.moves.{ FullMove, ValidMove }
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, Queen, Rook }

class MoveGenerator(board: Board, player: Color, history: Seq[FullMove]) {

  /**
   * A list of valid moves, except the ones that leave the king in check.
   * @return a list of full moves.
   */
  def allMoves(): List[FullMove] =
    validMovesWithoutCheck()
      .map(move => FullMove(move, resultsInCheck(move, player.other), resultsInCheckmate(move, player.other)))
      .filter(move => !resultsInCheck(move.validMove, player))

  private def validMovesWithoutCheck(): List[ValidMove] =
    board.pieces
      .filter(_._2.color == player)
      .flatMap { case (pos, piece) => piece.validMoves(pos, board, history) }
      .toList

  private def resultsInCheck(move: ValidMove, kingColor: Color): Boolean = {
    val afterBoard = board.apply(move.boardChanges)
    new MoveGenerator(afterBoard, kingColor.other, history)
      .validMovesWithoutCheck()
      .exists(capturesKing(_, kingColor, afterBoard))
  }

  private def resultsInCheckmate(move: ValidMove, kingColor: Color): Boolean = {
    val afterBoard     = board.apply(move.boardChanges)
    val afterGenerator = new MoveGenerator(afterBoard, kingColor, history)
    afterGenerator
      .validMovesWithoutCheck()
      .forall(afterGenerator.resultsInCheck(_, kingColor))
  }

  private def capturesKing(move: ValidMove, kingColor: Color, board: Board): Boolean =
    board
      .apply(move.boardChanges)
      .kingPos(kingColor)
      .isEmpty
}

object PieceGenerators {
  implicit class PieceWithMoveGenerator(piece: Piece) {
    def validMoves(from: Position, board: Board, history: Seq[FullMove]): List[ValidMove] = piece.pieceType match {
      case Knight => GenerateKnightMoves(piece.color, board, from)
      case Pawn =>
        GeneratePawnMoves(piece.color, board, from) ::: GeneratePawnSpecialMoves(piece.color, board, from, history)
      case Rook   => GenerateRookMoves(piece.color, board, from)
      case Bishop => GenerateBishopMoves(piece.color, board, from)
      case Queen  => GenerateQueenMoves(piece.color, board, from)
      case King   => GenerateKingMoves(piece.color, board, from)
    }
  }
}
