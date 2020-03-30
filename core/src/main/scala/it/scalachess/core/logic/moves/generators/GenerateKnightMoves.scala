package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ ValidMove, ValidSimpleMove }
import it.scalachess.core.pieces.PieceType
import scalaz.Validation

case class GenerateKnightMoves(pieceType: PieceType, color: Color, board: Board, from: Position)
    extends GeneratePieceMoves {

  override def apply(): List[ValidMove] =
    List(
      generateKnightMoves(Position.of(from.col + 1, from.row + 2)),
      generateKnightMoves(Position.of(from.col - 1, from.row + 2)),
      generateKnightMoves(Position.of(from.col + 1, from.row - 2)),
      generateKnightMoves(Position.of(from.col - 1, from.row - 2)),
      generateKnightMoves(Position.of(from.col + 2, from.row + 1)),
      generateKnightMoves(Position.of(from.col + 2, from.row - 1)),
      generateKnightMoves(Position.of(from.col - 2, from.row + 1)),
      generateKnightMoves(Position.of(from.col - 2, from.row - 1))
    ).filter(validation => validation.isSuccess)
      .map(_.toOption.get)

  private def generateKnightMoves(to: Option[Position]): Validation[String, ValidSimpleMove] =
    generatePieceMove(pieceType, color, board, from, to, "knight")

}
