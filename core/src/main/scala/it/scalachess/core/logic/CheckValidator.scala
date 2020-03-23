package it.scalachess.core.logic

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.Color
import scalaz.{ Failure, Success, Validation }

final case class CheckValidator(moveValidator: MoveValidator, testBoard: Board) {

  def validateMoveFromAllyKingCheck(currentPlayer: Color): Validation[String, Boolean] =
    testBoard kingPositionOf currentPlayer match {
      case Some(allyKingPos) =>
        Success(kingCanBeCaptured(allyKingPos, currentPlayer.other))
      case _ => Failure("ERROR: the ally king doesn't exist!")
    }

  def isOppositeKingInCheck(player: Color): Validation[String, Boolean] =
    testBoard kingPositionOf player.other match {
      case Some(oppositeKingPos) => Success(kingCanBeCaptured(oppositeKingPos, player))
      case _                     => Failure("ERROR: the opponent king doesn't exist!")
    }

  /*
  def isCheckMate(player: Color): Boolean = {
    val kingPos                     = board kingPositionOf player
    val kingPiece                   = board.pieceAtPosition(kingPos).toList.head
    val adiacentPos: List[Position] = kingPos
    val adiacentPosFree: List[Position] =
      adiacentPos.filter(pos =>
        if (kingPiece.canMove(kingPos, pos)) {
          moveValidator.validateShift(kingPos, pos, player) match {
            case Left(errorMsg)   => false
            case Right(validMove) => true
          }
        } else false)
    val adiacentPosThreathened: List[Position] =
      adiacentPos.filter(pos => kingCanBeCaptured(board, pos, player.other))
    println(adiacentPosFree)
    println(adiacentPosThreathened)
    if (adiacentPosFree.forall(pos => adiacentPosThreathened.contains(pos))) true
    else false
  }
   */

  private def kingCanBeCaptured(kingPosToCapture: Position, playerThatWantCapture: Color): Boolean =
    testBoard.pieces
      .filter(pieceEntry => {
        if (pieceEntry._2.color == playerThatWantCapture) {
          if (pieceEntry._2.canAttack(pieceEntry._1, kingPosToCapture)) {
            moveValidator.computePathError(pieceEntry._1, kingPosToCapture) match {
              case None =>
                true
              case _ => false
            }
          } else false
        } else false
      })
      .keySet
      .nonEmpty
}
