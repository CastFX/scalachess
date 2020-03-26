package it.scalachess.core.logic

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.{ Color }
import scalaz.{ Failure, Success, Validation }

final case class CheckValidator() {

  /**
   * Checks if the king is in play, and then verifies if it is under check
   * @param attackingPlayer the player that want to capture the king
   * @param testBoard the board where the KingCheck will be calculated
   * @param testMoveValidator
   * @return String if an error occur, otherwise the boolean that means the king check result
   */
  final def isKingInCheck(attackingPlayer: Color,
                          testBoard: Board,
                          testMoveValidator: MoveValidator): Validation[String, Boolean] =
    testBoard kingPositionOf attackingPlayer.other match {
      case Some(kingPosToCapture) =>
        Success(kingCanBeCaptured(kingPosToCapture, attackingPlayer, testBoard, testMoveValidator))
      case _ => Failure("ERROR: the king doesn't exist!")
    }

  /**
   * Verifies if the king is under check
   * @param kingPosToCapture the position of the king to capture
   * @param attackingPlayer the player that want to capture the king
   * @param testBoard the board where the king check will be verified
   * @param testMoveValidator
   * @return a boolean that means the king check result
   */
  private final def kingCanBeCaptured(kingPosToCapture: Position,
                                      attackingPlayer: Color,
                                      testBoard: Board,
                                      testMoveValidator: MoveValidator): Boolean =
    testBoard.pieces
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
   * @param testBoard the board where the king check mate will be verified
   * @param testMoveValidator
   * @return
   */
  final def isKingInCheckmate(attackingPlayer: Color, testBoard: Board, testMoveValidator: MoveValidator): Boolean =
    testBoard.pieces
      .filter(pieceEntry => pieceEntry._2.color == attackingPlayer.other)
      .flatMap(pieceEntry => pieceEntry._2.allPossibleValidMove(pieceEntry._1, testMoveValidator))
      .forall(validMove => {
        val nextBoard = testBoard(validMove)
        isKingInCheck(attackingPlayer, nextBoard, MoveValidator(nextBoard)) match {
          case Success(result) => result
        }
      })

}
