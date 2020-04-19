package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

import scala.annotation.tailrec

trait Minimax {

  final protected val checkmateValue = 9999.0

  /**
   * Generates the moves with the relative evaluation using the minimax algorithm.
   * @param board the board to evaluate (is the root node where the minimax search starts)
   * @param history the moves history played on the board (it completes the root node informations)
   * @param aiPlayer the color of the AI player
   * @param depth the depth used by the minimax algorithm
   * @param evaluationFunc the minimax will use this function to evaluate a node
   * @return the map containing the next possible moves and their relative valuation
   */
  final protected def minimax(board: Board,
                          history: Seq[FullMove],
                          aiPlayer: Color,
                          depth: Int,
                          evaluationFunc: (Board, Color) => Double): Map[FullMove, Double] = {
    require(depth > 0, "The minimax algorithm should computes at least the consequences of his first future move")
    new MoveGenerator(board, aiPlayer, history)
      .allMoves()
      .map(fullMove => {
        fullMove ->
          minimaxGoesDeep(board, history, depth, aiPlayer, aiPlayer, evaluationFunc, -checkmateValue, checkmateValue, fullMove)
      })
      .toMap
  }

  /**
   * Calls the minimax algorithm decreasing the depth.
   * @return the result of the minimax evaluation
   */
  protected def minimaxGoesDeep(board: Board, history: Seq[FullMove], depth: Int, currentPlayer: Color, maximizingPlayer: Color,
                                evaluationFunc: (Board, Color) => Double, alpha: Double, beta: Double, fullMove: FullMove): Double = {
    require(depth > 0, "Error: the minimax can't be called with a depth <= 0!")
    minimaxEvaluation(board(fullMove.validMove.boardChanges),
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
   * @return the evaluation of the board
   */
  private def minimaxEvaluation(board: Board,
                        history: Seq[FullMove],
                        depth: Int,
                        currentPlayer: Color,
                        maximizingPlayer: Color,
                        evaluationFunc: (Board, Color) => Double,
                        alpha: Double,
                        beta: Double): Double = {
    depth match {
      case 0 =>
        // an horizon node of minimax search
        evaluationFunc(board, maximizingPlayer)
      case _ =>
        val allPossibleMoves = new MoveGenerator(board, currentPlayer, history).allMoves()
        if (allPossibleMoves.isEmpty) {
          // a terminal node of minimax search - this move leads one of the 2 player in checkmate
          currentPlayerValue(currentPlayer, maximizingPlayer, checkmateValue, -checkmateValue)
        } else {
          // an intermediate node of minimax
          alphaBetaPruning(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alpha, beta,
            allPossibleMoves, currentPlayerValue(currentPlayer, maximizingPlayer, checkmateValue, -checkmateValue))
        }
    }
  }

  /**
   * Decides which value use during a minimax call.
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
   * Implements the alpha-beta pruning iterations.
   * @param depth the current depth of minimax algorithm
   * @param alpha
   * @param beta
   * @param allPossibleMoves all playable moves which needs to be evaluate during this alpha beta pruning iteration
   * @param bestMoveEval
   * @return
   */
  @tailrec
  final protected def alphaBetaPruning(board: Board, history: Seq[FullMove], depth: Int, currentPlayer: Color, maximizingPlayer: Color,
                                       evaluationFunc: (Board, Color) => Double, alpha: Double, beta: Double,
                                       allPossibleMoves: List[FullMove], bestMoveEval: Double): Double = {
    if(allPossibleMoves.isEmpty) bestMoveEval
    else {
      var bestMoveEvalUpdated = bestMoveEval
      val move = allPossibleMoves.head
      currentPlayer match {
        case `maximizingPlayer` =>
          bestMoveEvalUpdated = math.max(bestMoveEvalUpdated,
            minimaxGoesDeep(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alpha, beta, move))
        case _ =>
          bestMoveEvalUpdated = math.min(bestMoveEvalUpdated,
            minimaxGoesDeep(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alpha, beta, move))
      }
      val alphaOrBeta = currentPlayerValue(currentPlayer, maximizingPlayer, math.min(bestMoveEvalUpdated, beta), math.max(bestMoveEvalUpdated, alpha))
      currentPlayer match {
        case `maximizingPlayer` =>
          if (alphaOrBeta >= beta) bestMoveEvalUpdated
          else alphaBetaPruning(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alphaOrBeta, beta,
            allPossibleMoves.drop(1), bestMoveEvalUpdated)
        case _                  =>
          if (alpha >= alphaOrBeta) bestMoveEvalUpdated
          else alphaBetaPruning(board, history, depth, currentPlayer, maximizingPlayer, evaluationFunc, alpha, alphaOrBeta,
            allPossibleMoves.drop(1), bestMoveEvalUpdated)
      }
    }
  }

}
