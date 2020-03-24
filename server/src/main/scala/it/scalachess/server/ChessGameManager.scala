package it.scalachess.server

import akka.actor.{Actor, ActorRef}
import akka.event.{Logging, LoggingAdapter}
import it.scalachess.core.ChessGame
import it.scalachess.server.ChessGameManagerMessages.{Initialize, Join}
import it.scalachess.core.colors.Color

class LobbyManager extends Actor {
  val log: LoggingAdapter = Logging(context.system, this)

  var gameInfo: ChessGameInfo = ChessGameInfo(Seq(), ChessGame.standard())

  def receive: Receive = {
    case Initialize => initialize()
    case Join(_) => join(_)
  }

  private def initialize(): Unit = {
    gameInfo = ChessGameInfo(Seq(sender()), ChessGame.standard())
  }

  private def join(gameId: Int): Unit = {
    gameInfo = gameInfo.join(sender())
  }
}

object ChessGameManagerMessages {
  case class Initialize()
  case class Join(id: Int)
}