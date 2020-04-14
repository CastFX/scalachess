package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator
import scalaz.Validation

case class LevelTwo() extends Level {

  private val levelOne       = LevelOne()
  private val minimaxDepth   = 3
  private val checkmateValue = 9999.0

  override def apply(board: Board, aiPlayer: Color, history: Seq[FullMove]): Validation[String, FullMove] =
    if (isPlayerInCheckMate(board, aiPlayer.other, history)) {
      levelOne(board, aiPlayer, history)
    } else {
      moveWithMaxEvaluation(generateMovesWithMinimaxEval(board, aiPlayer, history, minimaxDepth))
    }

  private def isPlayerInCheckMate(board: Board, player: Color, history: Seq[FullMove]): Boolean =
    new MoveGenerator(board, player, history).allMoves().isEmpty

  private[level] def generateMovesWithMinimaxEval(board: Board,
                                                  aiPlayer: Color,
                                                  history: Seq[FullMove],
                                                  depth: Int): Map[FullMove, Double] = {
    require(depth > 0, "The minimax algorithm should computes at least the consequences of his first future move")
    new MoveGenerator(board, aiPlayer, history)
      .allMoves()
      .map(fullMove => {
        fullMove -> minimax(board(fullMove.validMove.boardChanges),
                            history :+ fullMove,
                            depth - 1,
                            aiPlayer.other,
                            aiPlayer)
      })
      .toMap
  }

  def minimax(board: Board,
              history: Seq[FullMove],
              depth: Int,
              activePlayer: Color,
              aiPlayerWhichMaximize: Color): Double = {
    def recallMinimax(possibleMoves: List[FullMove],
                      board: Board,
                      history: Seq[FullMove],
                      depth: Int,
                      activePlayer: Color,
                      aiPlayerWhichMaximize: Color): List[Double] =
      possibleMoves
        .map(move => {
          minimax(board(move.validMove.boardChanges),
                  history :+ move,
                  depth - 1,
                  activePlayer.other,
                  aiPlayerWhichMaximize)
        })
    depth match {
      case 0 => levelOne.evaluateBoardByPieceValue(board, aiPlayerWhichMaximize)
      case _ =>
        val possibleMoves = new MoveGenerator(board, activePlayer, history).allMoves()
        if (possibleMoves.isEmpty) {
          activePlayer match {
            case `aiPlayerWhichMaximize` => -checkmateValue // the move brings the ai player into checkmate
            case _                       => checkmateValue
          }
        } else {
          activePlayer match {
            case `aiPlayerWhichMaximize` =>
              recallMinimax(possibleMoves, board, history, depth, activePlayer, aiPlayerWhichMaximize).max
            case _ =>
              recallMinimax(possibleMoves, board, history, depth, activePlayer, aiPlayerWhichMaximize).min
          }
        }
    }
  }

}
