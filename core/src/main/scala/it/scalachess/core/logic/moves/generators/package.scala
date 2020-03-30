package it.scalachess.core.logic.moves

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.pieces.PieceType
import scalaz.{ Failure, Success, Validation }

import scala.annotation.tailrec

package object generators {

  def generatePieceMove(pieceType: PieceType,
                        color: Color,
                        board: Board,
                        from: Position,
                        to: Option[Position],
                        pieceName: String): Validation[String, ValidSimpleMove] =
    to match {
      case None => Failure(s"$pieceName's movement: the end position doesn't exist in the board")
      case Some(to) =>
        board.pieceAtPosition(to) match {
          case None => Success(ValidSimpleMove(pieceType, color, from, to, None))
          case Some(pieceToCapture) =>
            pieceToCapture.color match {
              case color.other => Success(ValidSimpleMove(pieceType, color, from, to, Some(pieceToCapture)))
              case _           => Failure(s"$pieceName's movement: can't attack an ally piece")
            }
        }
    }

  @tailrec
  def generateLinearMovement(pieceType: PieceType,
                             color: Color,
                             board: Board,
                             from: Position,
                             to: Option[Position],
                             pieceName: String,
                             colMod: Int,
                             rowMod: Int,
                             validMoves: List[ValidSimpleMove]): List[ValidMove] =
    generatePieceMove(pieceType, color, board, from, to, pieceName) match {
      case Failure(_) =>
        validMoves match { // the position is out of bound, or an ally piece has been find
          case validMoves: List[ValidMove] => validMoves
        }
      case Success(validSimpleMove) => // a move has been generated correctly
        validSimpleMove.capturedPiece match {
          case Some(_) =>
            (validMoves.::(validSimpleMove)) match { // the move which captures a piece is been generated
              case validMoves: List[ValidMove] => validMoves
            }
          case None => // processing the next position
            to match {
              case None =>
                validMoves match {
                  case validMoves: List[ValidMove] => validMoves
                }
              case Some(to) =>
                generateLinearMovement(pieceType,
                                       color,
                                       board,
                                       to,
                                       Position.of(to.col + colMod, to.row + rowMod),
                                       pieceName,
                                       rowMod,
                                       colMod,
                                       validMoves.::(validSimpleMove))

            }
        }
    }

}
