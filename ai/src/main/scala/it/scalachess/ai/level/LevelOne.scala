package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator
import it.scalachess.core.pieces.{ Bishop, King, Knight, Pawn, PieceType, Queen, Rook }
import scalaz.Validation

/**
 * The level one AI plays the move which capture the more important piece.
 */
case class LevelOne() extends Level {

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): Validation[String, FullMove] =
    moveWithMaxEvaluation(generateMovesWithEvaluation(board, aiPlayer, history))

  private[level] def generateMovesWithEvaluation(board: Board,
                                                 aiPlayer: Color,
                                                 history: Seq[FullMove]): Map[FullMove, Double] =
    new MoveGenerator(board: Board, aiPlayer: Color, history: Seq[FullMove])
      .allMoves()
      .map(move => move -> evaluateBoardByPieceValue(board(move.validMove.boardChanges), aiPlayer))
      .toMap

  private[level] def evaluateBoardByPieceValue(board: Board, player: Color): Double =
    board.pieces
      .map(piece =>
        piece._2.color match {
          case player.other => -pieceValue(piece._2.pieceType)
          case _            => pieceValue(piece._2.pieceType)
      })
      .toList
      .sum

  private def pieceValue(pieceType: PieceType): Double =
    pieceType match {
      case Pawn   => pawnValue
      case Knight => knightValue
      case Bishop => bishopValue
      case Rook   => rookValue
      case Queen  => queenValue
      case King   => kingValue
      case _ =>
        assert(assertion = false, s"The AI doesn't know the value of this piece: $pieceType")
        0
    }
  private val pawnValue   = 10
  private val knightValue = 30
  private val bishopValue = 35
  private val rookValue   = 50
  private val queenValue  = 100
  private val kingValue   = 1000

}
