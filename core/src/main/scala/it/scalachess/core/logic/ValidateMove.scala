package it.scalachess.core.logic

import it.scalachess.core.Color
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.pieces.{ Bishop, Pawn, Piece, PieceType, Queen, Rook }
import scalaz.{ Failure, Success, Validation }

final case class MoveValidator(board: Board) {

  private val checkValidator = CheckValidator()
  private val movesGenerator = MovesGenerator

  def validateMove() = {

  }

  /**
   * Create a ValidMove from a String representing a move
   * @param move the move as String
   * @param player the color of the active player
   * @return Success containing the ValidMove, otherwise a Failure with the error message
   */
  def validateSimpleMove(move: String, player: Color): Validation[String, simpleMove] =
    validateMoveFormat(move) match {
      case Success(positions) =>
        validateSimpleMove(positions._1, positions._2, player)
      case Failure(errorMsg) => Failure(errorMsg)
    }

  /**
   * Create a ValidMove from two positions
   * @param from starting position
   * @param to the final position
   * @param player the color of the active player
   * @return Success containing the ValidMove, otherwise a Failure with the error message
   */
  def validateSimpleMove(from: Position, to: Position, player: Color): Validation[String, simpleMove] =
    if (Position.of(from).nonEmpty && Position.of(to).nonEmpty)
      validateShift(from, to, player) match {
        case Success(piece) =>
          generatePathError(from, to) match {
            case None =>
              checkValidator.isKingInCheck(player.other, MoveValidator(board(simpleMove(from, to, piece)))) match {
                case Success(isAllyKingInCheck) =>
                  if (isAllyKingInCheck)
                    Failure("This move makes king under check!")
                  else
                    Success(simpleMove(from, to, piece))
                case Failure(errorMsg) => Failure(errorMsg)
              }
            case Some(errorMsg) => Failure(errorMsg)
          }
        case Failure(errorMsg) => Failure(errorMsg)
      } else Failure("Position inserted is out of bound!")

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

    def generateErrorInStraightPath(from: Position, to: Position) =
      if (from.colDistanceAbs(to) == 0)
        generateErrorPieceInPath(from.generatePosBetweenRow(to, Set()))
      else
        generateErrorPieceInPath(from.generatePosBetweenCol(to, Set()))

    def generateErrorInDiagonalPath(from: Position, to: Position) =
      generateErrorPieceInPath(from.generatePosBetweenDiagonal(to, Set()))

    board.pieceAtPosition(from) map (piece => piece.pieceType) getOrElse None match {
      case Rook   => generateErrorInStraightPath(from, to)
      case Bishop => generateErrorInDiagonalPath(from, to)
      case Queen =>
        if (from.colDistanceAbs(to) == 0 || from.rowDistanceAbs(to) == 0)
          generateErrorInStraightPath(from, to)
        else
          generateErrorInDiagonalPath(from, to)
      case Pawn =>
        if (from.colDistance(to) == 1)
          generateErrorInStraightPath(from, to)
        else None
      case _ => None
    }
  }

  /**
   * Checks if the move represented as String is represented as a valid format
   * @param move the move as String
   * @return Success containing a tuple defining a move, otherwise a Failure with the error message
   */
  private def validateMoveFormat(move: String): Validation[String, (Position, Position)] =
    if (move.length == 5) {
      (Position.ofNotation(move.substring(0, 2)), Position.ofNotation(move.substring(3, 5))) match {
        case (Some(from), Some(to)) => Success(from, to)
        case _ => Failure("Move format not legal")
      }
    } else {
      Failure("Move format not legal")
    }

}

case class simpleMove(from: Position, to: Position, piece: Piece)
