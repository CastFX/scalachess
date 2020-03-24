package it.scalachess.core.logic

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.Color
import it.scalachess.core.pieces.{ Bishop, Piece, Queen, Rook }
import scalaz.{ Failure, Success, Validation }

final case class MoveValidator(board: Board) {

  def validate(move: String, player: Color): Validation[String, ValidMove] =
    validateMoveFormat(move) match {
      case Success(positions) =>
        validate(positions._1, positions._2, player)
      case Failure(errorMsg) => Failure(errorMsg)
    }

  def validate(from: Position, to: Position, player: Color): Validation[String, ValidMove] =
    validateShift(from, to, player) match {
      case Success(piece) =>
        computePathError(from, to) match {
          case Some(errorMsg) => Failure(errorMsg)
          case None           => Success(ValidMove(from, to, piece))
        }
      case Failure(errorMsg) => Failure(errorMsg)
    }

  /**
   * Checks if the move represented as String format is located in the board.
   * @param move move represented as String
   * @return Either[String, (Position, Position)]
   *         is Left(String) when format is wrong,
   *         otherwise is Right[Position, Position] (representing a move)
   */
  private def validateMoveFormat(move: String): Validation[String, (Position, Position)] =
    (Position.ofNotation(move.substring(0, 2)), Position.ofNotation(move.substring(3, 5))) match {
      case (Some(from), Some(to)) => Success(from, to)
      case _                      => Failure("Move format not legal")
    }

  /**
   * Checks if only the two positions inserted represent a correct shift.
   * @param from position where is located the (active) player's piece
   * @param to position where the piece should move
   * @param player the active player
   * @return Either[String, Piece]
   *         is Left(String) when an error occur, otherwise is the Right[Piece] to move
   */
  def validateShift(from: Position, to: Position, player: Color): Validation[String, Piece] =
    board.pieceAtPosition(from) match {
      case Some(playerPiece) =>
        playerPiece.color match {
          case player =>
            board.pieceAtPosition(to) match {
              case None =>
                if (playerPiece.canMove(from, to)) {
                  if (from == to)
                    Failure("The piece is already in that position")
                  else
                    Success(playerPiece)
                } else Failure("The piece selected can't move there")
              case Some(color) =>
                color match {
                  case player => Failure("Can't capture an ally piece")
                  case _ =>
                    if (playerPiece.canAttack(from, to)) Success(playerPiece)
                    else Failure("The piece can't attacks there")
                }
            }
          case _ => Failure("Can't move an enemy piece")
        }
      case _ => Failure("The first position inserted is empty")
    }

  /**
   * Checks if the path crossed by the piece moved is free of pieces.
   * This method assume that the correctness of
   * the two end point positions is already checked.
   * @param from position where is located the (active) player's piece
   * @param to position where the piece should move
   * @return
   */
  def computePathError(from: Position, to: Position): Option[String] = {
    def computeErrorPieceInPath(path: Set[Position]) =
      if (path.forall(p => board.pieceAtPosition(p).isEmpty))
        None
      else Some("The piece can't move throught other pieces")
    def rookControl(from: Position, to: Position) =
      if (from.colDistanceAbs(to) == 0)
        computeErrorPieceInPath(from.computePosBetweenRow(to, Set()))
      else // if(from.rowDistanceAbs(to) == 0)
        computeErrorPieceInPath(from.computePosBetweenCol(to, Set()))
    def bishopControl(from: Position, to: Position) =
      computeErrorPieceInPath(from.computePosBetweenDiagonal(to, Set()))
    board.pieceAtPosition(from) map (piece => piece.pieceType) getOrElse (None) match {
      case Bishop => bishopControl(from, to)
      case Rook   => rookControl(from, to)
      case Queen =>
        if (from.colDistanceAbs(to) == 0)
          computeErrorPieceInPath(from.computePosBetweenRow(to, Set()))
        else if (from.rowDistanceAbs(to) == 0)
          computeErrorPieceInPath(from.computePosBetweenCol(to, Set()))
        else
          bishopControl(from, to)
      case _ => None
    }
  }

}

case class ValidMove(from: Position, to: Position, piece: Piece)
