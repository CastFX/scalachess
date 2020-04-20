package it.scalachess.ai.movesearch

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

import scala.annotation.tailrec

/**
 * Implements the minimax algorithm with alpha-beta pruning.
 */
trait Minimax {

  protected val minimaxDepth = 1
  final protected val checkmateValue = 9999.0

  /**
   * Generates the moves with the relative evaluation, calculated using the minimax algorithm.
   * @param board the board to evaluate (is the root node where the minimax search starts)
   * @param history the moves history played on the board (it completes the root node informations of the minimax search)
   * @param aiPlayer the color of the AI player
   * @param evaluationFunc the minimax will use this function to evaluate a node
   * @return the map containing the next possible moves and their relative valuation
   */
  final protected def minimax(board: Board,
                          history: Seq[FullMove],
                          aiPlayer: Color,
                          evaluationFunc: (Board, Color) => Double): Map[FullMove, Double] = {
    /**
     * The first minimax call on the root node.
     * @param depth the depth used by the minimax algorithm
     * @return
     */
    def minimaxRoot(board: Board,
                    history: Seq[FullMove],
                    aiPlayer: Color,
                    evaluationFunc: (Board, Color) => Double,
                    depth: Int): Map[FullMove, Double] = {
      require(depth > 0, "The minimax algorithm should computes at least the consequences of his first future move")
      new MoveGenerator(board, aiPlayer, history)
        .allMoves()
        .map(fullMove => {
          fullMove ->
            minimaxGoesDeep(board, history, depth, aiPlayer, aiPlayer, evaluationFunc, -checkmateValue, checkmateValue, fullMove)
        })
        .toMap
    }
    minimaxRoot(board, history, aiPlayer, evaluationFunc, minimaxDepth)
  }

  /**
   * Calls the minimax algorithm with alpha-beta pruning, decreasing the depth.
   * @return the result of the evaluation
   */
  protected def minimaxGoesDeep(board: Board, history: Seq[FullMove], depth: Int, currentPlayer: Color, maximizingPlayer: Color,
                                evaluationFunc: (Board, Color) => Double, alpha: Double, beta: Double, fullMove: FullMove): Double = {
    require(depth > 0, "Error: the minimax can't be called with a depth <= 0!")
    minimaxEval(board(fullMove.validMove.boardChanges),
      history :+ fullMove,
      depth - 1,
      currentPlayer.other,
      maximizingPlayer,
      evaluationFunc,
      alpha,
      beta)
  }

  /**
   * Evaluates a board relying on the minimax algorithm with alpha-beta pruning.
   * @param board the board to evaluate (is a node of the minimax search)
   * @param history the moves history played on the board (it completes the node informations of the minimax search)
   * @param depth the depth of the minimax algorithm
   * @param currentPlayer the active player color during the current minimax iteration
   * @param maximizingPlayer the color of the player that want to maximize the minimax evaluation (AI player)
   * @param evaluationFunc this function will be used to evaluate a node located on the minimax horizon
   * @param alpha alpha-beta pruning value
   * @param beta alpha-beta pruning value
   * @return the evaluation of the board
   */
  private def minimaxEval(board: Board,
                        history: Seq[FullMove],
                        depth: Int,
                        currentPlayer: Color,
                        maximizingPlayer: Color,
                        evaluationFunc: (Board, Color) => Double,
                        alpha: Double,
                        beta: Double): Double = {
    depth match {
      case 0 =>
        // an horizon node of the minimax search
        evaluationFunc(board, maximizingPlayer)
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
  }

  /**
   * Decides which value use during a minimax iteration.
   * @param currentPlayer the active player color during the current minimax iteration
   * @param maximizingPlayer the color of the player that want to maximize the minimax evaluation (AI player)
   * @param minimizingPlayerValue the value corresponding to the minimizing player
   * @param maximizingPlayerValue the value corresponding to the maximizing player
   * @return the value corresponding to the current player
   */
  final protected def currentPlayerValue(currentPlayer: Color,
                                   maximizingPlayer: Color,
                                   minimizingPlayerValue: Double,
                                   maximizingPlayerValue: Double): Double =
    currentPlayer match {
      case `maximizingPlayer` => maximizingPlayerValue
      case _                  => minimizingPlayerValue
    }

  /**
   * A unique procedure used to perform alpha-beta pruning, both for the minimizing player and for the maximizing player.
   * @param depth the current depth of minimax algorithm
   * @param currentPlayer the active player color during the current minimax iteration
   * @param maximizingPlayer the color of the player that want to maximize the minimax evaluation (AI player)
   * @param alpha alpha-beta pruning value: it is the current best move value calculated by the maximizing player
   * @param beta alpha-beta pruning value: it is the current worst move value calculated by the minimizing player
   * @param allPossibleMoves all playable moves that needs to be considered during this alpha-beta pruning iteration. Each one will lead to a new node
   * @param bestMoveEval the current best move value
   * @return
   */
  @tailrec
  final protected def alphaBetaPruningSearch(board: Board, history: Seq[FullMove], depth: Int, currentPlayer: Color, maximizingPlayer: Color,
                                             evaluationFunc: (Board, Color) => Double, alpha: Double, beta: Double,
                                             allPossibleMoves: Seq[FullMove], bestMoveEval: Double): Double = {
    if(allPossibleMoves.isEmpty) {
      // all nodes have been explored
      bestMoveEval
    } else {
      var bestMoveEvalUpdated = bestMoveEval
      val move = allPossibleMoves.head
      // 1° alpha-beta pruning step: updates the best value found so far
      currentPlayer match {
        case `maximizingPlayer` =>
          bestMoveEvalUpdated = math.max(bestMoveEvalUpdated,
            minimaxGoesDeep(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alpha, beta, move))
        case _ =>
          bestMoveEvalUpdated = math.min(bestMoveEvalUpdated,
            minimaxGoesDeep(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alpha, beta, move))
      }
      // 2° alpha-beta pruning step: updates alpha or beta valueS
      val alphaOrBeta = currentPlayerValue(currentPlayer, maximizingPlayer, math.min(bestMoveEvalUpdated, beta), math.max(bestMoveEvalUpdated, alpha))
      // 3° alpha-beta pruning phase: prune or continue the evaluation
      currentPlayer match {
        case `maximizingPlayer` =>
          if (alphaOrBeta >= beta) bestMoveEvalUpdated
          else alphaBetaPruningSearch(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alphaOrBeta, beta,
            allPossibleMoves.drop(1), bestMoveEvalUpdated)
        case _                  =>
          if (alpha >= alphaOrBeta) bestMoveEvalUpdated
          else alphaBetaPruningSearch(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alpha, alphaOrBeta,
            allPossibleMoves.drop(1), bestMoveEvalUpdated)
      }
    }
  }

}
