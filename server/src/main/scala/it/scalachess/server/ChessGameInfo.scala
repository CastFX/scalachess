package it.scalachess.server

import akka.actor.ActorRef
import it.scalachess.core.ChessGame

case class ChessGameInfo(players: Seq[ActorRef], game: ChessGame) {
  lazy val gameId: Int = hashCode()

  def join(secondPlayer: ActorRef): ChessGameInfo = {
    ChessGameInfo(players :+ secondPlayer, game)
  }
}
