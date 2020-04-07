package it.scalachess.client.local_client

import it.scalachess.client.view.{ CLI, View, ViewFactory }
import it.scalachess.core.{ ChessGame, Ongoing, Result, WinByForfeit }
import scalaz.{ Failure, Success }

object Client extends App {
  var game: ChessGame = ChessGame.standard()
  val view: View      = ViewFactory(CLI)

  while (game.gameStatus.equals(Ongoing)) {
    var waitingForValidMove = true
    view.showBoard(game.board)
    do {
      val move = scala.io.StdIn.readLine(s"It's ${game.player} turn to move, enter the move: ")
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
    waitingForValidMove = true
  }

  game.gameStatus match {
    case r: Result => view.showResult(r)
    case _         => ()
  }
}
