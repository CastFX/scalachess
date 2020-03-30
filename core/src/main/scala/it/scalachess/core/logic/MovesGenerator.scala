package it.scalachess.core.logic

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.generators.{
  GenerateBishopMoves,
  GenerateKnightMoves,
  GeneratePawnMoves,
  GenerateQueenMoves,
  GenerateRookMoves
}
import it.scalachess.core.logic.moves.ValidMove
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, Piece, Queen, Rook }

final case class MovesGenerator(board: Board, player: Color) {

  def apply(): Set[ValidMove] =
    board.pieces
      .filter(_._2.color == player)
      .flatMap(playerPiece => generateAllPossiblePieceMoves(playerPiece._1, playerPiece._2))
      .toSet

  def generateAllPossiblePieceMoves(from: Position, piece: Piece): Set[ValidMove] =
    piece.pieceType match {
      case Knight => GenerateKnightMoves(piece.pieceType, piece.color, board, from)()
      case Pawn   => GeneratePawnMoves(piece.pieceType, piece.color, board, from)()
      case Rook   => GenerateRookMoves(piece.pieceType, piece.color, board, from)()
      case Bishop => GenerateBishopMoves(piece.pieceType, piece.color, board, from)()
      case Queen  => GenerateQueenMoves(piece.pieceType, piece.color, board, from)()
      case King   => Set()
    }

  // TODO controllare lo scacco è molesto
  /*private def generateKingMoves(from: Position, piece: Piece): Set[ValidMove] =
    from.adjacentPositions
      .map(pos =>
        Position.of(pos) match {
          case Some(pos) => board.pieceAtPosition(pos)
      })
      .toSet
 */
}
