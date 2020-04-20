package it.scalachess.core.logic.moves

import it.scalachess.core.board.Position
import it.scalachess.core.pieces.PieceType

/**
 * An extension of a list of full move to implement the possibility of having specific filters
 */
object SeqFullMoveExtension {
  def apply[T <: FullMove](implicit a: SeqFullMove[T]): SeqFullMove[T] = a

  implicit class SeqFullMove[T <: FullMove](seq: Seq[T]) {
    def filterCastlings(byType: CastlingType): Seq[T] =
      seq.filter { fm =>
        fm.validMove match {
          case castling: ValidCastling => castling.castlingType == byType
          case _                       => false
        }
      }

    def filterPieces(pieceType: PieceType): Seq[T] =
      seq.filter(_.validMove.pieceType == pieceType)

    def filterPositions(to: Position, fromCol: Option[Char], fromRow: Option[Int]): Seq[T] =
      seq.filter {
        case FullMove(validMove, _, _, _) =>
          val endEquals = validMove.to == to
          val colEqualsIfPresent = fromCol.fold(true) { colChar =>
            validMove.from.col.equals(Position.colToInt(colChar))
          }
          val rowEqualsIfPresent = fromRow.fold(true) { row =>
            validMove.from.row.equals(row)
          }
          endEquals && colEqualsIfPresent && rowEqualsIfPresent
      }

    def filterChecks(isKingInCheck: Boolean, isKingInCheckmate: Boolean): Seq[T] =
      seq.filter {
        case FullMove(_, check, checkMate, _) =>
          (isKingInCheckmate == checkMate && checkMate) || (isKingInCheck == check && isKingInCheckmate == checkMate)
      }

    def filterCaptures(capture: Boolean): Seq[T] =
      seq.filter {
        case FullMove(validMove, _, _, _) =>
          (validMove.capture.isEmpty && !capture) || (validMove.capture.isDefined && capture)
      }

    def filterPromotions(promotion: Option[PieceType]): Seq[T] = promotion match {
      case Some(toPiece) =>
        seq.filter {
          case FullMove(ValidPromotion(_, _, _, promotesTo, _), _, _, _) => promotesTo.pieceType == toPiece
          case _                                                         => false
        }
      case None => seq
    }
  }
}
