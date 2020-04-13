package it.scalachess.ai.level

import it.scalachess.core.Color
import it.scalachess.core.board.Board
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.logic.moves.generators.MoveGenerator

case class LevelTwo() extends Level {

  private val levelOne = LevelOne()

  override def apply(board: Board, player: Color, history: Seq[FullMove]): FullMove = {
    val moves = generateMovesEvaluated(board, player, history, 3)
    moves.filter(move => {
      println(move._2 + "= valore della mossa: " + move._1)
      true
    })
    levelOne.moveWithMaxEvaluation(generateMovesEvaluated(board, player, history, 2))
  }

  def generateMovesEvaluated(board: Board, player: Color, history: Seq[FullMove], depth: Int): Map[FullMove, Double] = {
    def minimax(board: Board,
                history: Seq[FullMove],
                depth: Int,
                activePlayer: Color,
                maximizingPlayer: Color): Double = {

      def recallMinimax(board: Board,
                        history: Seq[FullMove],
                        depth: Int,
                        activePlayer: Color,
                        maximizingPlayer: Color): List[Double] =
        new MoveGenerator(board, activePlayer, history)
          .allMoves()
          .map(move => {
            minimax(board(move.validMove.boardChanges),
                    history :+ move,
                    depth - 1,
                    activePlayer.other,
                    maximizingPlayer)
          })

      depth match {
        case 0 => levelOne.evaluateBoardByPieceValue(board, maximizingPlayer)
        case _ =>
          activePlayer match {
            case maximizingPlayer.other =>
              val result = recallMinimax(board, history, depth, activePlayer, maximizingPlayer)
              if (result.nonEmpty) result.min
              else 9999
            case _ =>
              val result = recallMinimax(board, history, depth, activePlayer, maximizingPlayer)
              if (result.nonEmpty) result.max
              else -9999
          }
      }
    }

    require(depth > 0, "The AI should compute at least the consequences his first future move")
    new MoveGenerator(board, player, history)
      .allMoves()
      .map(move =>
        move -> minimax(board(move.validMove.boardChanges), history :+ move, depth - 1, player.other, player))
      .toMap
  }

}
