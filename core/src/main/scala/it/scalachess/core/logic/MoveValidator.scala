package it.scalachess.core.logic

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.Color
import it.scalachess.core.pieces.{ Bishop, Piece, Queen, Rook }

final case class MoveValidator(board: Board) {

  // TODO perhaps change Either[String, ValidMove] to Validation[String, ValidMove]
  def computeMoveErrors(move: String, player: Color): Either[String, ValidMove] =
    computeMoveFormat(move) match {
      case Left(errorMsg) => Left(errorMsg)
      case Right(positionsTuple) =>
        computeShiftError(positionsTuple._1, positionsTuple._2, player) match {
          case Left(errorMsg) => Left(errorMsg)
          case Right(piece) =>
            computePathError(positionsTuple._1, positionsTuple._2) match {
              case Some(errorMsg) => Left(errorMsg)
              case None           => Right(ValidMove(positionsTuple._1, positionsTuple._2, piece))
            }
        }
    }

  /**
   * Checks if the move represented as String format is located in the board.
   * @param move move represented as String
   * @return Either[String, (Position, Position)]
   *         is Left(String) when format is wrong,
   *         otherwise is Right[Position, Position] (representing a move)
   */
  // TODO change Either to Validation
  private def computeMoveFormat(move: String): Either[String, (Position, Position)] =
    (Position.ofNotation(move.substring(0, 2)), Position.ofNotation(move.substring(3, 5))) match {
      case (Some(from), Some(to)) => Right(from, to)
      case _                      => Left("Move format not legal")
    }

  /**
   * Checks if only the two positions inserted represent a correct shift.
   * @param from position where is located the (active) player's piece
   * @param to position where the piece should move
   * @param player the active player
   * @return Either[String, Piece]
   *         is Left(String) when an error occur, otherwise is the Right[Piece] to move
   */
  def computeShiftError(from: Position, to: Position, player: Color): Either[String, Piece] =
    board.pieceAtPosition(from) match {
      case None => Left("The first position inserted is empty")
      case Some(playerPiece) =>
        if (playerPiece.color == player.other) Left("Can't move an enemy piece")
        else {
          board.pieceAtPosition(to) match {
            case None =>
              if (playerPiece.canMove(from, to)) Right(playerPiece)
              else Left("The piece selected can't move there")
            case Some(color) =>
              if (color == player) Left("Can't capture an ally piece")
              else {
                if (playerPiece.canAttack(from, to)) Right(playerPiece)
                else Left("The piece can't attacks there")
              }
          }
        }
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
