package it.scalachess.core.logic

import it.scalachess.core.board.Position
import it.scalachess.core.pieces.PieceType

final case class MovesGenerator() {

  def apply(parsedMove: ParsedMove): Option[ParsedMove] = {
    parsedMove match {
      case Castling(_) || EnPassant(_) || Promotion(_) =>
        if (generateSpecialMoves contains parsedMove) Some(parsedMove)
        else None
      case Move(endPos, pieceType, capture, check, checkmate, startingCol, startingRow, promotion) =>
        if (generateSimpleMoves contains parsedMove) Some(parsedMove)
        else None)
    }
  }

  private def generateSimpleMoves(piece: PieceType, from: Position): Set[ParsedMove] = {

  }

  private def generateSpecialMoves(): Set[ParsedMove] = {
    ???
  }

}
