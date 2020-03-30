package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.CheckValidator
import it.scalachess.core.logic.moves.{ ValidMove, ValidSimpleMove }
import it.scalachess.core.pieces.PieceType
import scalaz.{ Failure, Success, Validation }

case class GenerateKingMoves(pieceType: PieceType, color: Color, board: Board, from: Position)
    extends GeneratePieceMoves {

  private val checkValidator = CheckValidator()

  override def apply(): List[ValidMove] =
    List() ++
    from.adjacentPositions
      .flatMap(pos => generateKingMoves(Position.of(pos)).toOption)

  private def generateKingMoves(to: Option[Position]): Validation[String, ValidSimpleMove] =
    generatePieceMove(pieceType, color, board, from, to, "King") match {
      case Failure(errorMsg) => Failure(errorMsg)
      case Success(validMove: ValidMove) =>
        board(validMove.convertInBoardMove) match {
          case Failure(errorMsg) => Failure(errorMsg)
          case Success(board) =>
            checkValidator.isKingInCheck(color.other, board) match {
              case Failure(errorMsg) => Failure(errorMsg)
              case Success(result) =>
                if (result) Failure("applying that move: the king is under check")
                else Success(validMove)
            }
        }

    }

}
