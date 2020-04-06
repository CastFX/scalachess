package it.scalachess.core.logic.moves.generators

import it.scalachess.core.{ Black, Color, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ ValidMove, ValidPromotion, ValidSimpleMove }
import it.scalachess.core.pieces.{ Bishop, Knight, Pawn, Piece, Queen, Rook }

private[generators] object PawnMoves extends PieceMoves {

  override def apply(color: Color, board: Board, from: Position): List[ValidMove] =
    color match {
      case White => pawnSimpleMoves(color, board, from, from.rowUpMod, Board.whitePawnsStartingRow)
      case Black => pawnSimpleMoves(color, board, from, from.rowDownMod, Board.blackPawnsStartingRow)
    }

  def pawnSimpleMoves(color: Color,
                      board: Board,
                      from: Position,
                      forwardRowMod: Int,
                      startingRow: Int): List[ValidMove] = {
    val moveOnePosWithoutCapture =
      pawnMovement(color, board, from, Position.of(from.col, from.row + forwardRowMod))
    val moveTwoPosWithoutCapture =
      from.row match {
        case `startingRow` =>
          moveOnePosWithoutCapture match {
            case None => None
            case Some(_) =>
              pawnMovement(color, board, from, Position.of(from.col, from.row + forwardRowMod + forwardRowMod))
          }
        case _ =>
          None // can't move two position forward, because it's not located in the starting row")
      }
    val leftAttack =
      pawnAttack(color, board, from, Position.of(from.col - from.colLeftMod, from.row + forwardRowMod))
    val rightAttack =
      pawnAttack(color, board, from, Position.of(from.col + from.colRightMod, from.row + forwardRowMod))
    List(moveOnePosWithoutCapture, moveTwoPosWithoutCapture, leftAttack, rightAttack)
      .filter(_.nonEmpty)
      .map(_.get)
      .flatMap(move => {
        if (isPromoting(move))
          convertToPromotion(move)
        else
          List(move)
      })
  }

  private def pawnMovement(color: Color, board: Board, from: Position, to: Option[Position]): Option[ValidSimpleMove] =
    to match {
      case None => None // the end position doesn't exist in the board
      case Some(to) =>
        board.pieceAtPosition(to) match {
          case None    => Some(ValidSimpleMove(from, to, Pawn, color, None))
          case Some(_) => None // pawn can't pass over pieces
        }
    }

  private def pawnAttack(color: Color, board: Board, from: Position, to: Option[Position]): Option[ValidSimpleMove] =
    to match {
      case None => None // the end position doesn't exist in the board
      case Some(to) =>
        board.pieceAtPosition(to) match {
          case None => None // there's no piece to attack in the end position
          case Some(pieceToCapture) =>
            pieceToCapture.color match {
              case color.other => Some(ValidSimpleMove(from, to, Pawn, color, Some(to)))
              case _           => None // can't attack an ally piece
            }
        }
    }

  private def convertToPromotion(move: ValidSimpleMove): List[ValidPromotion] = {
    val symbols = Seq(Queen, Knight, Bishop, Rook)
    symbols
      .map(piece => ValidPromotion(move.from, move.to, move.color, Piece(move.color, piece), Some(move.to)))
      .toList
  }

  private def isPromoting(move: ValidSimpleMove): Boolean =
    (move.color == White && move.to.row == 8) || (move.color == Black && move.to.row == 1)

}
