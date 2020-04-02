package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.IsKingInCheck
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, PieceType, Queen, Rook }
import scalaz.{ Failure, Success, Validation }

final case class MovesGenerator(player: Color, board: Board) {

  def apply(isKingExposed: Boolean): List[ValidMove] = {
    val validMovesFreeFromCheck = board.pieces
      .filter(_._2.color == player)
      .flatMap(playerPiece => generateAllPossiblePieceMoves(playerPiece._1, playerPiece._2.pieceType)) //Map Generated moves to ValidMoves con check e checkmate
      .toList
    if (isKingExposed) validMovesFreeFromCheck.filter(move => validateMoveFromKingCheck(move, player, board).isSuccess)
    else validMovesFreeFromCheck
  }

  private def generateAllPossiblePieceMoves(from: Position, pieceType: PieceType): List[ValidMove] = //GeneratedMoves
    pieceType match {
      case Knight => GenerateKnightMoves(pieceType, player, board, from)
      case Pawn =>
        GeneratePawnMoves(pieceType, player, board, from) // ::: GeneratePawnSpecialMoves(pieceType, player, board, from)
      case Rook   => GenerateRookMoves(pieceType, player, board, from)
      case Bishop => GenerateBishopMoves(pieceType, player, board, from)
      case Queen  => GenerateQueenMoves(pieceType, player, board, from)
      case King   => GenerateKingMoves(pieceType, player, board, from)
    }

  private def validateMoveFromKingCheck(validMove: ValidMove,
                                        exposedPlayer: Color,
                                        board: Board): Validation[String, ValidMove] =
    if (IsKingInCheck(exposedPlayer, board(validMove.convertInBoardMove))) Failure("king is in check")
    else Success(validMove)

}
