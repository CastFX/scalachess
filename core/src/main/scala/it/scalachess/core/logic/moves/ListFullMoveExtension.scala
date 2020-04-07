package it.scalachess.core.logic.moves

import it.scalachess.core.board.Position
import it.scalachess.core.pieces.PieceType

/**
 * An extension of a list of full move to implement the possibility of having specific filters
 */
object ListFullMoveExtension {
  def apply[T <: FullMove](implicit a: ListFullMove[T]): ListFullMove[T] = a

  implicit class ListFullMove[T <: FullMove](list: List[T]) {
    def filterCastlings(byType: CastlingType): List[T] =
      list.filter { fm =>
        fm.validMove match {
          case castling: ValidCastling => castling.castlingType == byType
          case _                       => false
        }
      }

    def filterPieces(pieceType: PieceType): List[T] =
      list.filter(_.validMove.pieceType == pieceType)

    def filterPositions(to: Position, fromCol: Option[Char], fromRow: Option[Int]): List[T] =
      list.filter {
        case FullMove(validMove, _, _) =>
          val endEquals = validMove.to == to
          val colEqualsIfPresent = fromCol.fold(true) { colChar =>
            validMove.from.col.equals(Position.colToInt(colChar))
          }
          val rowEqualsIfPresent = fromRow.fold(true) { row =>
            validMove.from.row.equals(row)
          }
          endEquals && colEqualsIfPresent && rowEqualsIfPresent
      }

    def filterChecks(isKingInCheck: Boolean, isKingInCheckmate: Boolean): List[T] =
      list.filter {
        case FullMove(_, check, checkMate) =>
          (isKingInCheckmate == checkMate && checkMate) || (isKingInCheck == check && isKingInCheckmate == checkMate)
      }

    def filterCaptures(capture: Boolean): List[T] =
      list.filter {
        case FullMove(validMove, _, _) =>
          (validMove.capture.isEmpty && !capture) || (validMove.capture.isDefined && capture)
      }

    def filterPromotions(promotion: Option[PieceType]): List[T] = promotion match {
      case Some(toPiece) =>
        list.filter {
          case FullMove(ValidPromotion(_, _, _, promotesTo, _), _, _) => promotesTo.pieceType == toPiece
          case _                                                      => false
        }
      case None => list
    }
  }
}
