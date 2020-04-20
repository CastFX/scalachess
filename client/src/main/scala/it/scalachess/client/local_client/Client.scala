package it.scalachess.client.local_client

import it.scalachess.ai.AI
import it.scalachess.client.view.{CLI, View, ViewFactory}
import it.scalachess.core._
import it.scalachess.core.logic.{Ongoing, Result, WinByForfeit}
import it.scalachess.core.logic.moves.FullMove
import it.scalachess.core.parser.NonAmbiguousGameSaver
import scalaz.{Failure, Success}

import scala.io.StdIn.readLine

object Client extends App {
  var game: ChessGame = ChessGame.standard()
  val view: View      = ViewFactory(CLI)
  val aiLevelRegex    = "ai ([0-9])".r
  val aiLevels        = 0 to AI.maxDifficulty

  view.showMessage(
    s"Type 'ai [level]' to play against an AI or 'p2' to play against another human. Available AI levels: ${aiLevels
      .mkString(",")}")
  val optionAI: Option[AI] = readLine() match {
    case aiLevelRegex(l) =>
      val lvl = l.toInt
      if (lvl > AI.maxDifficulty) {
        view.showMessage(s"AI level not existing, playing against another human")
        None
      } else {
        view.showMessage(s"Playing against AI level $lvl")
        view.showMessage(s"You start as White")
        Some(AI(lvl.toInt, Black))
      }
    case _ =>
      view.showMessage("Playing against another human")
      None
  }

  while (game.gameStatus.equals(Ongoing)) {
    var waitingForValidMove = true
    view.showBoard(game.board)
    do {
      view.showMessage(s"It's ${game.player} turn to move, enter the move: ")
      val move = readLine().trim
      move match {
        case "/forfeit" =>
          game = game.end(WinByForfeit(game.player.other))
          waitingForValidMove = false
        case _ =>
          game(move) match {
            case Failure(e) =>
              view.showMessage(s"error = $e")
            case Success(updatedGame) =>
              game = updatedGame
              waitingForValidMove = false
          }
      }
    } while (waitingForValidMove)

    optionAI match {
      case Some(ai) if game.gameStatus.equals(Ongoing) =>
        view.showBoard(game.board)
        view.showMessage("WAIT AI TURN")
        val move: FullMove = ai.generateSmartMove(game.board, game.moveHistory)
        game = game(move)
      case _ => ()
    }
  }

  game.gameStatus match {
    case r: Result =>
      val pgn = NonAmbiguousGameSaver.convertAndFormat(game.moveHistory, Some(r))
      view.showMessage(s"PGN:\n$pgn")
      view.showResult(r)
    case _         => ()
  }
}
