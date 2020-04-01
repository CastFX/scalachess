package it.scalachess.core.logic.moves.generators

import it.scalachess.core.{ Black, Color, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ ValidMove, ValidSimpleMove }
import it.scalachess.core.pieces.PieceType
import scalaz.{ Failure, Success, Validation }

private[generators] object GeneratePawnMoves extends GeneratePieceMoves {

  override def apply(pieceType: PieceType, color: Color, board: Board, from: Position): List[ValidMove] =
    color match {
      case White => generatePawnSimpleMoves(pieceType, color, board, from, from.rowUpMod, Board.whitePawnsStartingRow)
      case Black => generatePawnSimpleMoves(pieceType, color, board, from, from.rowDownMod, Board.blackPawnsStartingRow)
    }

  def generatePawnSimpleMoves(pieceType: PieceType,
                              color: Color,
                              board: Board,
                              from: Position,
                              forwardRowMod: Int,
                              startingRow: Int): List[ValidSimpleMove] = {
    val moveOnePosWithoutCapture =
      generatePawnMovement(pieceType, color, board, from, Position.of(from.col, from.row + forwardRowMod))
    val moveTwoPosWithoutCapture =
      from.row match {
        case `startingRow` =>
          moveOnePosWithoutCapture match {
            case Failure(errorMsg) => Failure(errorMsg)
            case Success(_) =>
              generatePawnMovement(pieceType,
                                   color,
                                   board,
                                   from,
                                   Position.of(from.col, from.row + forwardRowMod + forwardRowMod))
          }
        case _ =>
          Failure("Pawn's movement: can't move two position forward, because it's not located in the starting row")
      }
    val leftAttack =
      generatePawnAttack(pieceType,
                         color,
                         board,
                         from,
                         Position.of(from.col - from.colLeftMod, from.row + forwardRowMod))
    val rightAttack =
      generatePawnAttack(pieceType,
                         color,
                         board,
                         from,
                         Position.of(from.col + from.colRightMod, from.row + forwardRowMod))
    List(moveOnePosWithoutCapture, moveTwoPosWithoutCapture, leftAttack, rightAttack)
      .filter(_.toOption.nonEmpty)
      .map(_.toOption.get)
  }

  private def generatePawnMovement(pieceType: PieceType,
                                   color: Color,
                                   board: Board,
                                   from: Position,
                                   to: Option[Position]): Validation[String, ValidSimpleMove] =
    to match {
      case None => Failure("Pawn's movement: the end position doesn't exist in the board")
      case Some(to) =>
        board.pieceAtPosition(to) match {
          case None    => Success(ValidSimpleMove(pieceType, color, from, to, None))
          case Some(_) => Failure("Pawn's movement: can't pass over pieces")
        }
    }

  private def generatePawnAttack(pieceType: PieceType,
                                 color: Color,
                                 board: Board,
                                 from: Position,
                                 to: Option[Position]): Validation[String, ValidSimpleMove] =
    to match {
      case None => Failure("Pawn's attack: the end position doesn't exist in the board")
      case Some(to) =>
        board.pieceAtPosition(to) match {
          case None => Failure("Pawn's attack: there's no piece in the end position")
          case Some(pieceToCapture) =>
            pieceToCapture.color match {
              case color.other => Success(ValidSimpleMove(pieceType, color, from, to, Some(pieceToCapture)))
              case _           => Failure("Pawn's attack: can't attack an ally piece")
            }
        }
    }

}
