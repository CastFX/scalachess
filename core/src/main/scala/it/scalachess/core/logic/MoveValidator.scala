package it.scalachess.core.logic

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.Color
import it.scalachess.core.pieces.{ Bishop, Piece, Queen, Rook }

import scala.annotation.tailrec

final case class MoveValidator(board: Board) {

  // TODO change Either to Validation
  def computeMoveErrors(move: String, player: Color): Either[String, ValidMove] =
    computeMoveFormat(move) match {
      case Left(errorMsg) => Left(errorMsg)
      case Right(positionsTuple) =>
        computeShiftError(positionsTuple._1, positionsTuple._2, player) match {
          case Left(errorMsg) => Left(errorMsg)
          case Right(piece) =>
            computePathError(positionsTuple._1, positionsTuple._2, player) match {
              case Some(errorMsg) => Left(errorMsg)
              case None           => Right(ValidMove(positionsTuple._1, positionsTuple._2, piece))
            }
        }
    }

  /**
   * Checks if the move represented as String format is located in the board
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
   * Checks if only the two positions inserted represent a correct shift
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

  // TODO this method need refactoring: method too long + legibility/Optimization)
  def computePathError(from: Position, to: Position, player: Color): Option[String] = {
    // TODO: imho this method should be inside the Position class (to be Discuss with Serra)
    @tailrec
    def getColPosBetween(from: Position, to: Position, posOnPath: List[Position]): List[Position] = {
      val distance: Int = to.effectiveColDistance(from)
      distance match {
        case 1 | -1 => posOnPath
        case diff if diff < -1 =>
          val backwardPosInPath = Position(from.col - 1, from.row)
          getColPosBetween(backwardPosInPath, to, posOnPath.::(backwardPosInPath))
        case _ =>
          val forwardPosInPath = Position(from.col + 1, from.row)
          getColPosBetween(forwardPosInPath, to, posOnPath.::(forwardPosInPath))
      }
    }
    // TODO: imho this method should be inside the Position class (to be Discuss with Serra)
    @tailrec
    def getRowPosBetween(from: Position, to: Position, path: List[Position]): List[Position] =
      to effectiveRowDistance from match {
        case 1 | -1 => path
        case diff if diff < -1 =>
          val backwardPosInPath = Position(from.col, from.row - 1)
          getRowPosBetween(backwardPosInPath, to, path.::(backwardPosInPath))
        case _ =>
          val forwardPosInPath = Position(from.col, from.row + 1)
          getRowPosBetween(forwardPosInPath, to, path.::(forwardPosInPath))
      }
    // TODO: imho this method should be inside the Position class (to be Discuss with Serra)
    @tailrec
    def getDiagonalPosBetween(from: Position, to: Position, path: List[Position]): List[Position] =
      to.effectiveRowDistance(from) match {
        case 1 | -1 => path
        case diff if diff < -1 =>
          val backwardPosInPath = Position(from.col - 1, from.row - 1)
          getDiagonalPosBetween(backwardPosInPath, to, path.::(backwardPosInPath))
        case _ =>
          val forwardPosInPath = Position(from.col + 1, from.row + 1)
          getDiagonalPosBetween(forwardPosInPath, to, path.::(forwardPosInPath))
      }
    def computePieceInPath(path: List[Position]) =
      if (path
            .filter(position => board.pieceAtPosition(position).getOrElse(false) == Piece)
            .isEmpty) None
      else Some("The piece can't move throught other pieces")

    board pieceAtPosition from map (piece => piece.pieceType) getOrElse (None) match {
      case Rook =>
        if (from.colDistance(to) == 0)
          computePieceInPath(getRowPosBetween(from, to, List[Position]()))
        else
          computePieceInPath(getColPosBetween(from, to, List[Position]()))
      case Bishop =>
        computePieceInPath(getDiagonalPosBetween(from, to, List[Position]()))
      case Queen =>
        if (from.colDistance(to) > 0 && from.rowDistance(to) > 0
            && from.colDistance(to) == from.rowDistance(to))
          computePieceInPath(getDiagonalPosBetween(from, to, List[Position]()))
        else if (from.colDistance(to) == 0)
          computePieceInPath(getRowPosBetween(from, to, List[Position]()))
        else if (from.rowDistance(to) == 0) {
          computePieceInPath(getColPosBetween(from, to, List[Position]()))
        } else Some("the queen can't move there")
      case _ => None
    }
  }

}

case class ValidMove(from: Position, to: Position, piece: Piece)
