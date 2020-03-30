package it.scalachess.core.logic

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.pieces.{ King, Piece }
import scalaz.{ Failure, Success, Validation }

final case class CheckValidator() {

  /**
   * Checks if the king is in play, and then verifies if it is under check
   * @param attackingPlayer the player that want to capture the king
   * @param board in which calculate the check
   * @return String if an error occur, otherwise the boolean that means the king check result
   */
  def isKingInCheck(attackingPlayer: Color, board: Board): Validation[String, Boolean] =
    board.pieces
      .find {
        case (_, Piece(color, king)) => color == attackingPlayer.other && king == King
      }
      .flatMap(kingEntry => Some(kingEntry._1)) match {
      case Some(kingPos) =>
        // println("kingPos = " + kingPos + ", attackingPlayer = " + attackingPlayer + ", board = " + board) // TODO remove
        Success(kingCanBeCaptured(kingPos, attackingPlayer, MoveValidator(board)))
      case _ => Failure("ERROR: the king doesn't exist!")
    }

  /**
   * Verifies if the king is under check
   * @param kingPosToCapture the position of the king to capture
   * @param attackingPlayer the player that want to capture the king
   * @param testMoveValidator that will assure if the king is on check
   * @return true if the king can be captured
   */
  private def kingCanBeCaptured(kingPosToCapture: Position,
                                attackingPlayer: Color,
                                testMoveValidator: MoveValidator): Boolean =
    testMoveValidator.board.pieces
      .exists(pieceEntry => {
        if (pieceEntry._2.color == attackingPlayer) {
          if (testMoveValidator
                .pieceCanAttack(pieceEntry._2.pieceType, pieceEntry._2.color, pieceEntry._1, kingPosToCapture)) {
            testMoveValidator.generatePathError(pieceEntry._1, kingPosToCapture) match {
              case None =>
                true
              case _ => false
            }
          } else false
        } else false
      })

  /*
  // TODO alternative check code, but cause stackoverflow
    private def kingCanBeCaptured(kingPosToCapture: Position,
                                attackingPlayer: Color,
                                board: Board): Boolean =
      MovesGenerator(board, attackingPlayer)()
        .exists {
          case validMove: ValidSimpleMove =>
            validMove.capturedPiece match {
              case None => false
              case Some(piece) =>
                if (piece == Piece(attackingPlayer.other, King)) true
                else false
            }
        }
   */

  /**
   * Verifies if the king is under check mate
   * @param attackingPlayer the player that want to capture the king
   * @param testMoveValidator that will assure if the king is on check
   * @return true if the king is on checkmate
   */
  def isKingInCheckmate(attackingPlayer: Color, testMoveValidator: MoveValidator): Boolean =
    testMoveValidator.board.pieces
      .filter(pieceEntry => pieceEntry._2.color == attackingPlayer.other)
      .flatMap(pieceEntry =>
        MovesGenerator(testMoveValidator.board, attackingPlayer.other)
          .generateAllPossiblePieceMoves(pieceEntry._1, pieceEntry._2.pieceType))
      .forall(validMove => {
        testMoveValidator.board.apply(validMove.convertInBoardMove) match {
          case Success(board) =>
            isKingInCheck(attackingPlayer, board) match {
              case Success(result) => {
                if (result == false) {
                  println("sta mossa dà falso " + validMove)
                }
                result
              }
            }
        }
      })

}
