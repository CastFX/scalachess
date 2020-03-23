package it.scalachess.core.logic

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.Color
import it.scalachess.core.pieces.{ Bishop, Piece, Queen, Rook }
import scalaz.{ Failure, Success, Validation }

final case class MoveValidator(board: Board) {

  def validate(move: String, player: Color): Validation[String, ValidMove] =
    computeMoveFormat(move) match {
      case Success(positions) =>
        validateShift(positions._1, positions._2, player) match {
          case Success(piece) =>
            computePathError(positions._1, positions._2) match {
              case Some(errorMsg) => Failure(errorMsg)
              case None           => Success(ValidMove(positions._1, positions._2, piece))
            }
          case Failure(errorMsg) => Failure(errorMsg)
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
  private def computeMoveFormat(move: String): Validation[String, (Position, Position)] =
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
        if (playerPiece.color.name == player.other) Failure("Can't move an enemy piece")
        else {
          board.pieceAtPosition(to) match {
            case None =>
              if (playerPiece.canMove(from, to)) Success(playerPiece)
              else Failure("The piece selected can't move there")
            case Some(color) =>
              if (color == player) Failure("Can't capture an ally piece")
              else {
                if (playerPiece.canAttack(from, to)) Success(playerPiece)
                else Failure("The piece can't attacks there")
              }
          }
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
    def computeErrorPieceInPath(path: List[Position]) =
      if (path
            .filter(position => board.pieceAtPosition(position).getOrElse(false) == Piece)
            .isEmpty) None
      else Some("The piece can't move throught other pieces")
    def rookControl(from: Position, to: Position) =
      if (from.colDistanceAbs(to) == 0)
        computeErrorPieceInPath(from.computeRowPosBetween(to, List()))
      else // if(from.rowDistanceAbs(to) == 0)
        computeErrorPieceInPath(from.computeColPosBetween(to, List()))
    def bishopControl(from: Position, to: Position) =
      computeErrorPieceInPath(from.computeDiagonalPosBetween(to, List()))
    board.pieceAtPosition(from) map (piece => piece.pieceType) getOrElse (None) match {
      case Bishop => bishopControl(from, to)
      case Rook   => rookControl(from, to)
      case Queen =>
        if (from.colDistanceAbs(to) == 0)
          computeErrorPieceInPath(from.computeRowPosBetween(to, List()))
        else if (from.rowDistanceAbs(to) == 0)
          computeErrorPieceInPath(from.computeColPosBetween(to, List()))
        else
          bishopControl(from, to)
      case _ => None
    }
  }

}

case class ValidMove(from: Position, to: Position, piece: Piece)
