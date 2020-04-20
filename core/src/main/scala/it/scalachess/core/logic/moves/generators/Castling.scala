package it.scalachess.core.logic.moves.generators

import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves._
import it.scalachess.core.pieces.{ King, Rook }
import it.scalachess.core.{ Black, Color, White }

private[generators] object Castling extends PieceSpecialMoves {
  private val whiteRow = 1
  private val blackRow = 8

  private val kingCol      = 5
  private val leftRookCol  = 1
  private val rightRookCol = 8

  private val queenSideRookCol = 4
  private val queenSideKingCol = 3
  private val kingSideRookCol  = 6
  private val kingSideKingCol  = 7

  val whiteQueenSide = ValidCastling(Position(kingCol, whiteRow),
                                     Position(queenSideKingCol, whiteRow),
                                     White,
                                     Position(leftRookCol, whiteRow),
                                     Position(queenSideRookCol, whiteRow),
                                     QueenSide)
  val whiteKingSide = ValidCastling(Position(kingCol, whiteRow),
                                    Position(kingSideKingCol, whiteRow),
                                    White,
                                    Position(rightRookCol, whiteRow),
                                    Position(kingSideRookCol, whiteRow),
                                    KingSide)
  val blackQueenSide = ValidCastling(Position(kingCol, blackRow),
                                     Position(queenSideKingCol, blackRow),
                                     Black,
                                     Position(leftRookCol, blackRow),
                                     Position(queenSideRookCol, blackRow),
                                     QueenSide)
  val blackKingSide = ValidCastling(Position(kingCol, blackRow),
                                    Position(kingSideKingCol, blackRow),
                                    Black,
                                    Position(rightRookCol, blackRow),
                                    Position(kingSideRookCol, blackRow),
                                    KingSide)

  override def apply(color: Color, board: Board, from: Position, history: Seq[FullMove]): Seq[ValidMove] =
    castlings(color, board, from, history)

  private def castlings(color: Color, board: Board, position: Position, history: Seq[FullMove]): Seq[ValidMove] = {
    val moves     = history.map(_.validMove)
    val isInCheck = history.lastOption.nonEmpty && history.last.resultsInCheck
    val kingMoved = history.exists(m => m.validMove.pieceType == King && m.validMove.color == color)
    val castlingAlreadyDone = moves.exists { move =>
      move.isInstanceOf[ValidCastling] && move.color == color
    }
    color match {
      case _ if isInCheck || castlingAlreadyDone || kingMoved => Seq()
      case White =>
        Seq(validateCastling(whiteKingSide, board, history), validateCastling(whiteQueenSide, board, history)).flatten
      case Black =>
        Seq(validateCastling(blackKingSide, board, history), validateCastling(blackQueenSide, board, history)).flatten
    }
  }

  private def validateCastling(move: ValidCastling, board: Board, history: Seq[FullMove]): Option[ValidCastling] =
    if (!rookMoved(move.color, move.rookFrom.col, history)
        && emptyBetween(move.rookFrom, move.from, board)) Some(move)
    else None

  private def emptyBetween(rook: Position, king: Position, board: Board): Boolean =
    rowPositionInBetween(rook, king).forall(pos => board.pieceAtPosition(pos).isEmpty)

  private def rowPositionInBetween(rook: Position, king: Position): Seq[Position] =
    if (rook.col < king.col)
      for (i <- rook.col + 1 until king.col) yield Position(i, rook.row)
    else
      for (i <- king.col + 1 until rook.col) yield Position(i, rook.row)

  private def rookMoved(player: Color, rookCol: Int, history: Seq[FullMove]): Boolean =
    history.exists(m => m.validMove.pieceType == Rook && m.validMove.color == player && m.validMove.from.col == rookCol)
}
