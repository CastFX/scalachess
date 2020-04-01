package it.scalachess.core.logic.moves

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.pieces.PieceType
import scalaz.{ Failure, Success, Validation }

import scala.annotation.tailrec

package object generators {

  def generateSimpleMove(pieceType: PieceType,
                         player: Color,
                         board: Board,
                         from: Position,
                         to: Option[Position]): Validation[String, ValidSimpleMove] =
    to match {
      case None => Failure(s"${pieceType.name}'s movement: the end position doesn't exist in the board")
      case Some(to) =>
        board.pieceAtPosition(to) match {
          case None => Success(ValidSimpleMove(pieceType, player, from, to, None))
          case Some(pieceToCapture) =>
            pieceToCapture.color match {
              case player.other => Success(ValidSimpleMove(pieceType, player, from, to, Some(pieceToCapture)))
              case _            => Failure(s"${pieceType.name}'s movement: can't attack an ally piece")
            }
        }
    }

  @tailrec
  def generateLinearMovementSimpleMoves(pieceType: PieceType,
                                        color: Color,
                                        board: Board,
                                        from: Position,
                                        to: Option[Position],
                                        colMod: Int,
                                        rowMod: Int,
                                        simpleValidMoves: List[ValidSimpleMove]): List[ValidSimpleMove] =
    generateSimpleMove(pieceType, color, board, from, to) match {
      case Failure(_) => simpleValidMoves // the position is out of bound, or an ally piece has been find
      case Success(validMove) =>
        validMove match {
          case validSimpleMove: ValidSimpleMove =>
            validSimpleMove.capturedPiece match {
              case Some(_) => simpleValidMoves.::(validSimpleMove) // the move which captures a piece is been generated
              case None => // processing the next position
                to match {
                  case Some(to) =>
                    generateLinearMovementSimpleMoves(pieceType,
                                                      color,
                                                      board,
                                                      from,
                                                      Position.of(to.col + colMod, to.row + rowMod),
                                                      colMod,
                                                      rowMod,
                                                      simpleValidMoves.::(validSimpleMove))
                  case _ => simpleValidMoves
                }
            }
        }
    }

}
