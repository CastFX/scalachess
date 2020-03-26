package it.scalachess.client.remoteClient

import akka.actor.typed.Behavior
import akka.actor.typed.scaladsl.Behaviors

object Client {

  sealed trait ClientCommand
  final case class StartGame()
  final case class

  def apply(): Behavior[T] = {
    Behaviors.receive { (context, message) =>

    }
  }

  private def
}

