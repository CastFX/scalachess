package it.scalachess.core.logic

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.colors.Color
import it.scalachess.core.pieces.{ Bishop, Piece, Queen, Rook }
import scalaz.{ Failure, Success, Validation }

final case class MoveValidator(board: Board) {

  private val checkValidator = CheckValidator()

  /**
   * Create a ValidMove from a String representing a move
   * @param move the move as String
   * @param player the color of the active player
   * @return Success containing the ValidMove, otherwise a Failure with the error message
   */
  def validateMove(move: String, player: Color): Validation[String, ValidMove] =
    validateMoveFormat(move) match {
      case Success(positions) =>
        validateMoveWithError(positions._1, positions._2, player)
      case Failure(errorMsg) => Failure(errorMsg)
    }

  /**
   * Create a ValidMove from two positions
   * @param from the starting position
   * @param to the final position
   * @param player the color of the active player
   * @return Success containing the ValidMove, otherwise a Failure with the error message
   */
  def validateMoveWithError(from: Position, to: Position, player: Color): Validation[String, ValidMove] =
    if (isPositionInBound(from) && isPositionInBound(to))
      validateShift(from, to, player) match {
        case Success(piece) =>
          generatePathError(from, to) match {
            case None =>
              val nextBoard = board(ValidMove(from, to, piece))
              checkValidator.isKingInCheck(player.other, nextBoard, MoveValidator(nextBoard)) match {
                case Success(isAllyKingInCheck) =>
                  if (isAllyKingInCheck)
                    Failure("This move makes king under check!")
                  else
                    Success(ValidMove(from, to, piece))
                case Failure(errorMsg) => Failure(errorMsg)
              }
            case Some(errorMsg) => Failure(errorMsg)
          }
        case Failure(errorMsg) => Failure(errorMsg)
      } else Failure("Position inserted is out of bound!")

  /**
   * Create a ValidMove from two positions
   * @param from the starting position
   * @param to the final position
   * @param player the color of the active player
   * @return Option containing the ValidMove
   */
  def validateMove(from: Position, to: Position, player: Color): Option[ValidMove] =
    if (isPositionInBound(from) && isPositionInBound(to))
      validateShift(from, to, player) match {
        case Success(piece) =>
          generatePathError(from, to) match {
            case None =>
              val nextBoard = board(ValidMove(from, to, piece))
              checkValidator.isKingInCheck(player.other, nextBoard, MoveValidator(nextBoard)) match {
                case Success(isAllyKingInCheck) =>
                  if (isAllyKingInCheck)
                    None
                  else
                    Some(ValidMove(from, to, piece))
                case Failure(errorMsg) => None
              }
            case _ => None
          }
        case _ => None
      } else None

  /**
   * Checks if only the two end point positions represent a correct move
   * @param from   position where is located the (active) player's piece
   * @param to     position where the piece should move
   * @param player the active player
   * @return Success containing the Piece to move, otherwise a Failure with the error message
   */
  def validateShift(from: Position, to: Position, player: Color): Validation[String, Piece] =
    board.pieceAtPosition(from) match {
      case None => Failure("The first position inserted is empty")
      case Some(playerPiece) =>
        playerPiece.color match {
          case player.other => Failure("Can't move an enemy piece")
          case _ =>
            board.pieceAtPosition(to) match {
              case None =>
                if (playerPiece.canMove(from, to)) Success(playerPiece)
                else Failure("The piece selected can't move there")
              case Some(piece) =>
                piece.color match {
                  case player.other =>
                    if (playerPiece.canAttack(from, to)) Success(playerPiece)
                    else Failure("The piece can't attacks there")
                  case _ => Failure("Can't capture an ally piece")
                }
            }
        }
    }

  /**
   * Checks if the path crossed by the piece moved is free of pieces.
   * This method assume that the correctness of the two end point positions is already checked.
   * @param from the start position where is located the (active) player's piece
   * @param to   the final position where the piece should move
   * @return Option containing the error message
   */
  def generatePathError(from: Position, to: Position): Option[String] = {

    def generateErrorPieceInPath(path: Set[Position]) =
      if (path.forall(p => board.pieceAtPosition(p).isEmpty)) None
      else Some("The piece can't move throught other pieces")

    def generateRookError(from: Position, to: Position) =
      if (from.colDistanceAbs(to) == 0)
        generateErrorPieceInPath(from.generatePosBetweenRow(to, Set()))
      else
        generateErrorPieceInPath(from.generatePosBetweenCol(to, Set()))

    def generateBishopError(from: Position, to: Position) =
      generateErrorPieceInPath(from.generatePosBetweenDiagonal(to, Set()))

    board.pieceAtPosition(from) map (piece => piece.pieceType) getOrElse (None) match {
      case Bishop => generateBishopError(from, to)
      case Rook   => generateRookError(from, to)
      case Queen =>
        if (from.colDistanceAbs(to) == 0)
          generateErrorPieceInPath(from.generatePosBetweenRow(to, Set()))
        else if (from.rowDistanceAbs(to) == 0)
          generateErrorPieceInPath(from.generatePosBetweenCol(to, Set()))
        else
          generateBishopError(from, to)
      case _ => None
    }
  }

  /**
   * Checks if the move represented as String is represented as a valid format
   * @param move the move as String
   * @return Success containing a tuple defining a move, otherwise a Failure with the error message
   */
  private def validateMoveFormat(move: String): Validation[String, (Position, Position)] =
    (Position.ofNotation(move.substring(0, 2)), Position.ofNotation(move.substring(3, 5))) match {
      case (Some(from), Some(to)) => Success(from, to)
      case _                      => Failure("Move format not legal")
    }

  /**
   * Checks if the position inserted exists in the board
   * @param pos the position to check
   * @return true if it's in the board
   */
  private def isPositionInBound(pos: Position): Boolean =
    if (pos.col > Board.width || pos.col < 1 || pos.row > Board.height || pos.row < 1) false
    else true

}

case class ValidMove(from: Position, to: Position, piece: Piece)
