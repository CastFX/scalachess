package it.scalachess.ai.movesearch

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator


/**
 * Extends the minimax algorithm with quiescence search.
 */
trait MinimaxWithQuiescence extends Minimax {

  protected val quiescenceSearchActive = true
  protected val nodeNotQuietValue = 10 // it must be a significant value

  /**
   * Calls the minimax algorithm with alpha-beta pruning and quiescence search, decreasing the depth.
   * @return the result of the evaluation
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
      board)
  }

  /**
   * Evaluates a board relying on the minimax algorithm with alpha-beta pruning and quiescence search.
   * @param oldBoard the parent node that must be passed to the quiescence search function
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
                                    oldBoard: Board): Double =
    depth match {
      case 0 =>
        // an horizon node of the minimax search
        if(quiescenceSearchActive) quiescenceSearchOneDepth(board, history, maximizingPlayer, evaluationFunc, oldBoard)
        else evaluationFunc(board, maximizingPlayer)
      case _ =>
        val allPossibleMoves = new MoveGenerator(board, currentPlayer, history).allMoves()
        if (allPossibleMoves.isEmpty) {
          // a terminal node of minimax search: one of the two player is in checkmate
          currentPlayerValue(currentPlayer, maximizingPlayer, checkmateValue, -checkmateValue)
        } else {
          // an intermediate node of the minimax search
          alphaBetaPruningSearch(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alpha, beta,
            allPossibleMoves, currentPlayerValue(currentPlayer, maximizingPlayer, checkmateValue, -checkmateValue))
        }
    }

  /**
   * Executes the quiescence search when founds a 'non quiet' node. A 'non quiet' node is a node where a capture happen
   * as last move. The quiescence search consists in evaluating all the possibles next nodes.
   * @param board the current node
   * @param oldBoard the father node
   * @return
   */
  final protected def quiescenceSearchOneDepth(board: Board, history: Seq[FullMove], maximizingPlayer: Color,
                                               evaluationFunc: (Board, Color) => Double, oldBoard: Board): Double =
    minimaxDepth % 2 match {
      case 0 =>
        // the minimax ends the evaluation on the minimizing player's moves: no needs to verify the opponent's quiescence
        evaluationFunc(board, maximizingPlayer)
      case _ =>
        // checks if the node is quiet and acts accordingly
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