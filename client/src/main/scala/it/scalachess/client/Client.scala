package it.scalachess.client

import it.scalachess.client.view.{ CLI, View, ViewFactory }
import it.scalachess.core.{ ChessGame, Ongoing }
import scalaz.{ Failure, Validation }
import com.typesafe.scalalogging.Logger
import it.scalachess.core.parser.Parser.{ AlgebraicParser, Parser }

object Client extends App {
  var game: ChessGame                       = ChessGame.standard()
  val view: View                            = ViewFactory(CLI)
  var move: String                          = ""
  var result: Validation[String, ChessGame] = scalaz.Failure("Empty result")
  val logger: Logger                        = Logger("Main")
  val parser: AlgebraicParser               = AlgebraicParser()

  while (game.gameStatus.equals(Ongoing)) {
    view.showBoard(game.board)
    do {
      move = scala.io.StdIn.readLine(s"Its ${game.player} turn to move, enter the move: ")
      parser.parse(move) match {
        case None => logger.error("Not algebraic format, insert another move ")
        case Some(parsedMove) => {
          result = game(parsedMove)
          result match {
            case Failure(e) => logger.error(s"error = $e")
            case _          => logger.debug("The move was accepted")
          }
        }
      }
    } while (result.isFailure)
    game = result.getOrElse(game)
  }
  logger.debug(s"Game over ${game.gameStatus}")
}
