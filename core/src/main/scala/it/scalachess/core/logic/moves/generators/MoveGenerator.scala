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
      .map { move =>
        val boardAfter = board.apply(move.boardChanges)
        val check      = resultsInCheck(move, player.other)
        val checkMate  = check && resultsInCheckmate(move, player.other)
        FullMove(move, check, checkMate, boardAfter)
      }
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
      case Knight =>
        KnightMoves(piece.color, board, from)
      case Pawn =>
        PawnMoves(piece.color, board, from) ::: PawnSpecialMoves(piece.color, board, from, history)
      case Rook   => RookMoves(piece.color, board, from)
      case Bishop => BishopMoves(piece.color, board, from)
      case Queen  => QueenMoves(piece.color, board, from)
      case King =>
        KingMoves(piece.color, board, from) ::: KingSpecialMoves(piece.color, board, from, history)
    }
  }
}
