package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator


/**
* The level two A.I.'s strategy is try to play the move which capture the more important piece,
 * but before proceed with capture it will consider the opponent next move.
 */
class LevelTwo() extends LevelOne {

  protected val quiescenceSearchActive = true
  protected val minimaxDepth = 1
  protected val nodeNotQuietValue = 10 // it must be a significant value

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): FullMove = {
    verifyGameIsPlayable(board, aiPlayer, history)
    moveWithMaxEval(minimax(board, history, aiPlayer, minimaxDepth, evaluatePiecesInBoard))
  }

  /**
   * Calls the minimax quiescence algorithm decreasing the depth.
   * @return the result of the minimax quiescence evaluation
   */
  final override protected def minimaxGoesDeep(board: Board, history: Seq[FullMove], depth: Int, currentPlayer: Color, maximizingPlayer: Color,
                                         evaluationFunc: (Board, Color) => Double, alpha: Double, beta: Double, fullMove: FullMove): Double = {
    require(depth > 0, "Error: the minimax can't be called with a depth <= 0!")
    minimaxQuiescenceEval(board(fullMove.validMove.boardChanges),
      history :+ fullMove,
      depth - 1,
      currentPlayer.other,
      maximizingPlayer,
      evaluationFunc,
      alpha,
      beta,
      board,
      fullMove)
  }

  /**
   * Evaluates a board relying on the minimax algorithm with alpha-beta pruning and quiescence search.
   * @param board the board to evaluate (is a node of the minimax search)
   * @param history the moves history played on the board (it completes the node informations of the minimax search)
   * @param depth the depth of the minimax algorithm
   * @param currentPlayer the active player color during the current minimax iteration
   * @param maximizingPlayer the color of the player that want to maximize the minimax evaluation (AI player)
   * @return the evaluation of the board
   */
  private def minimaxQuiescenceEval(board: Board,
                               history: Seq[FullMove],
                               depth: Int,
                               currentPlayer: Color,
                               maximizingPlayer: Color,
                               evaluationFunc: (Board, Color) => Double,
                               alpha: Double,
                               beta: Double,
                               oldBoard: Board,
                               fullMove: FullMove): Double =
      depth match {
        case 0 => // an horizon node of minimax search
          if(quiescenceSearchActive) quiescenceSearchOneDepth(board, history, maximizingPlayer, evaluationFunc, oldBoard)
          else evaluationFunc(board, maximizingPlayer)
        case _ =>
          val allPossibleMoves = new MoveGenerator(board, currentPlayer, history).allMoves()
          if (allPossibleMoves.isEmpty) { // a terminal node of minimax search - this move leads one of the 2 player in checkmate
            currentPlayerValue(currentPlayer, maximizingPlayer, checkmateValue, -checkmateValue)
          } else { // an intermediate node of minimax
            alphaBetaPruning(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alpha, beta,
              allPossibleMoves, currentPlayerValue(currentPlayer, maximizingPlayer, checkmateValue, -checkmateValue))
          }
      }

  final protected def quiescenceSearchOneDepth(board: Board, history: Seq[FullMove], maximizingPlayer: Color,
                                         evaluationFunc: (Board, Color) => Double, oldBoard: Board): Double =
    minimaxDepth % 2 match {
      case 0 =>
        // the minimax ends the evaluation on the minimizing player's moves: no needs to verify the opponent's quiescence
        evaluationFunc(board, maximizingPlayer)
      case _ =>
        val currentEvaluation = evaluationFunc(board, maximizingPlayer)
        if (currentEvaluation - evaluationFunc(oldBoard, maximizingPlayer) >= nodeNotQuietValue) {
          val minimizingPlayerMovesEval = new MoveGenerator(board, maximizingPlayer.other, history)
            .allMoves()
            .map(fullMove => evaluationFunc(board(fullMove.validMove.boardChanges), maximizingPlayer))
          if (minimizingPlayerMovesEval.isEmpty) checkmateValue
          else minimizingPlayerMovesEval.min
        } else currentEvaluation
    }

}
