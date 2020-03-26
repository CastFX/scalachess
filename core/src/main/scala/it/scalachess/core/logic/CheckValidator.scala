package it.scalachess.core.logic

import it.scalachess.core.Color
import it.scalachess.core.board.Position
import scalaz.{ Failure, Success, Validation }

final case class CheckValidator() {

  /**
   * Checks if the king is in play, and then verifies if it is under check
   * @param attackingPlayer the player that want to capture the king
   * @param testMoveValidator that will assure if the king is on check
   * @return String if an error occur, otherwise the boolean that means the king check result
   */
  def isKingInCheck(attackingPlayer: Color, testMoveValidator: MoveValidator): Validation[String, Boolean] =
    testMoveValidator.board.kingPositionOf(attackingPlayer.other) match {
      case Some(kingPosToCapture) =>
        Success(kingCanBeCaptured(kingPosToCapture, attackingPlayer, testMoveValidator))
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
          if (pieceEntry._2.canAttack(pieceEntry._1, kingPosToCapture)) {
            testMoveValidator.generatePathError(pieceEntry._1, kingPosToCapture) match {
              case None =>
                true
              case _ => false
            }
          } else false
        } else false
      })

  /**
   * Verifies if the king is under check mate
   * @param attackingPlayer the player that want to capture the king
   * @param testMoveValidator that will assure if the king is on check
   * @return true if the king is on checkmate
   */
  def isKingInCheckmate(attackingPlayer: Color, testMoveValidator: MoveValidator): Boolean =
    testMoveValidator.board.pieces
      .filter(pieceEntry => pieceEntry._2.color == attackingPlayer.other)
      .flatMap(pieceEntry => pieceEntry._2.allPossibleValidMove(pieceEntry._1, testMoveValidator))
      .forall(validMove => {
        isKingInCheck(attackingPlayer, MoveValidator(testMoveValidator.board(validMove))) match {
          case Success(result) => result
        }
      })

}
