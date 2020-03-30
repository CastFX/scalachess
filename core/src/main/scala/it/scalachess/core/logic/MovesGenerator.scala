package it.scalachess.core.logic

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.generators.{
  GenerateBishopMoves,
  GenerateKingMoves,
  GenerateKnightMoves,
  GeneratePawnMoves,
  GenerateQueenMoves,
  GenerateRookMoves
}
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, PieceType, Queen, Rook }

final case class MovesGenerator(board: Board, player: Color) {

  def apply(): List[ValidMove] =
    board.pieces
      .filter(_._2.color == player)
      .flatMap(playerPiece => generateAllPossiblePieceMoves(playerPiece._1, playerPiece._2.pieceType))
      .toList

  def generateAllPossiblePieceMoves(from: Position, pieceType: PieceType): List[ValidMove] =
    pieceType match {
      case Knight => GenerateKnightMoves(pieceType, player, board, from)()
      case Pawn   => GeneratePawnMoves(pieceType, player, board, from)()
      case Rook   => GenerateRookMoves(pieceType, player, board, from)()
      case Bishop => GenerateBishopMoves(pieceType, player, board, from)()
      case Queen  => GenerateQueenMoves(pieceType, player, board, from)()
      case King   => GenerateKingMoves(pieceType, player, board, from)()
    }

}
