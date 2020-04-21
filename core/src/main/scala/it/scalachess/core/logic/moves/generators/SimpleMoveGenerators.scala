package it.scalachess.core.logic.moves.generators

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.ValidSimpleMove
import it.scalachess.core.pieces.PieceType

import scala.annotation.tailrec

object SimpleMoveGenerators {

  def simpleMove(pieceType: PieceType,
                 player: Color,
                 board: Board,
                 from: Position,
                 to: Option[Position]): Option[ValidSimpleMove] =
    to match {
      case None => None // the end position doesn't exist in the board
      case Some(to) =>
        board.pieceAtPosition(to) match {
          case None => Some(ValidSimpleMove(from, to, pieceType, player, None))
          case Some(pieceToCapture) =>
            pieceToCapture.color match {
              case player.other => Some(ValidSimpleMove(from, to, pieceType, player, Some(to)))
              case _            => None // can't attack an ally piece")
            }
        }
    }

  def linearMovement(pieceType: PieceType,
                     color: Color,
                     board: Board,
                     from: Position,
                     to: Option[Position],
                     colMod: Int,
                     rowMod: Int): List[ValidSimpleMove] = {
    @tailrec
    def linearMovementRec(pieceType: PieceType,
                          color: Color,
                          board: Board,
                          from: Position,
                          to: Option[Position],
                          colMod: Int,
                          rowMod: Int,
                          simpleValidMoves: List[ValidSimpleMove]): List[ValidSimpleMove] =
      simpleMove(pieceType, color, board, from, to) match {
        case None => simpleValidMoves // the position is out of bound, or an ally piece has been find
        case Some(validMove) =>
          validMove match {
            case validSimpleMove: ValidSimpleMove =>
              validSimpleMove.capture match {
                case Some(_) =>
                  simpleValidMoves :+ validSimpleMove // the move which captures a piece is been generated
                case None => // processing the next position
                  to match {
                    case Some(to) =>
                      linearMovementRec(pieceType,
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
    linearMovementRec(pieceType: PieceType,
                      color: Color,
                      board: Board,
                      from: Position,
                      to: Option[Position],
                      colMod: Int,
                      rowMod: Int,
                      List())
  }

}
