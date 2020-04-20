package it.scalachess.ai.level

import it.scalachess.ai.movesearch.{Minimax, MinimaxNode}
import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.pieces.{Bishop, King, Knight, Pawn, PieceType, Queen, Rook}

/**
 * The level one AI plays a move which capture the more important piece.
 */
class LevelOne() extends LevelZero with Minimax {

  override def apply(board: Board, history: Seq[FullMove], aiPlayer: Color): FullMove = {
    verifyGameIsPlayable(board, history, aiPlayer)
    moveWithMaxEval(minimax(MinimaxNode(board, history), aiPlayer, evaluateBoardByPieces))
  }

  /**
   * Returns the move having the max evaluation from the input seq.
   * If two or more moves have the same evaluation, the move is choosen randomly.
   * @param movesEvaluated the map containing the moves and their evaluation
   * @return a (random) move having the higher evaluation
   */
  final protected def moveWithMaxEval(movesEvaluated: Map[FullMove, Double]): FullMove = {
    val maxEvaluation = movesEvaluated.values.max
    randomMove(movesEvaluated.filter(_._2 == maxEvaluation).keys.toSeq)
  }

  /**
   * Evaluates board relying on pieces' type importance and the player's color.
   * @param board the board to evaluate
   * @param aiPlayer the color of the AI player
   * @return the evaluation of the board
   */
  final protected def evaluateBoardByPieces(board: Board, aiPlayer: Color): Double =
    board.pieces
      .map(piece =>
        piece._2.color match {
          case `aiPlayer` => evaluatePiece(piece._2.pieceType)
          case _          => -evaluatePiece(piece._2.pieceType)
      })
      .toList
      .sum

  /**
   * Evaluates a piece relying on his type.
   * @param pieceType the piece type to evaluate
   * @return the evalutation of that piece type
   */
  final protected def evaluatePiece(pieceType: PieceType): Double =
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
  final protected val pawnValue   = 10
  final protected val knightValue = 30
  final protected val bishopValue = 35
  final protected val rookValue   = 50
  final protected val queenValue  = 100
  final protected val kingValue   = 1000

}
