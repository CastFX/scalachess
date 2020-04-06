package it.scalachess.core.logic.moves.generators

import it.scalachess.core.{ Black, Color, White }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ CastlingType, FullMove, KingSide, QueenSide, ValidCastling, ValidMove }

private[generators] object KingSpecialMoves extends PieceSpecialMoves {

  override def apply(color: Color, board: Board, from: Position, history: Seq[FullMove]): List[ValidMove] =
    castling(color, board, from, history)

  private def castling(color: Color, board: Board, position: Position, history: Seq[FullMove]): List[ValidMove] = {
    val moves = history.map(_.validMove)
    if (history.lastOption.nonEmpty && history.last.resultsInCheck) List() //Cannot be done if king is in check
    else {
      if (moves
            .flatMap {
              case castling: ValidCastling => Some(castling)
              case _                       => None
            }
            .count(x => x.color == color) != 0) List() //Cannot be done if castling already done
      else {
        color match {
          case White =>
            val rookLeft  = Rook(White, Position(1, 1), Position(4, 1))
            val rookRight = Rook(White, Position(8, 1), Position(6, 1))
            val kingLeft  = King(White, Position(5, 1), Position(3, 1))
            val kingRight = King(White, Position(5, 1), Position(7, 1))
            getCastling(rookLeft, kingLeft, moves, color, board, QueenSide) :::
            getCastling(rookRight, kingRight, moves, color, board, KingSide)
          case Black =>
            val rookLeft  = Rook(Black, Position(1, 8), Position(4, 8))
            val rookRight = Rook(Black, Position(8, 8), Position(6, 8))
            val kingLeft  = King(Black, Position(5, 8), Position(3, 8))
            val kingRight = King(Black, Position(5, 8), Position(7, 8))
            getCastling(rookLeft, kingLeft, moves, color, board, QueenSide) :::
            getCastling(rookRight, kingRight, moves, color, board, KingSide)
        }
      }
    }
  }

  private case class Rook(color: Color, from: Position, to: Position)

  private case class King(color: Color, from: Position, to: Position)

  private def rowPositionInBetween(rook: Position, king: Position): Seq[Position] =
    if (rook.col < king.col)
      for (i <- rook.col + 1 until king.col) yield Position(i, rook.row)
    else
      for (i <- king.col + 1 until rook.col) yield Position(i, rook.row)

  private def canGoThrough(positions: Seq[Position], board: Board): Boolean =
    positions.forall(pos => board.pieceAtPosition(pos).isEmpty)

  private def getCastling(rook: Rook,
                          king: King,
                          moves: Seq[ValidMove],
                          color: Color,
                          board: Board,
                          castlingType: CastlingType): List[ValidMove] =
    if (moves.count(x => x.pieceType.name == "King" && x.color == color) == 0 && //King must have never been moved
        moves.count(x => x.from == rook.from && x.color == color) == 0 &&        //Rook must have never been moved
        canGoThrough(rowPositionInBetween(rook.from, king.from), board)) //No pieces between King and Rook
      List(ValidCastling(king.from, king.to, color, rook.from, rook.to, castlingType))
    else {
      List()
    }
}
