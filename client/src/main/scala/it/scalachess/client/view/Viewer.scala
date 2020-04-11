package it.scalachess.client.view

import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed.{ ActorRef, Behavior }
import it.scalachess.client.view.ViewCommands.{ ShowBoard, ShowMessage, ShowResult, ViewMessage }
import it.scalachess.core.Result
import it.scalachess.core.board.Board
import it.scalachess.util.NetworkMessages.ClientMessage

/**
 * Companion Object of the Actor in charge of showing the view to the Client.
 */
object Viewer {

  def apply(parent: ActorRef[ClientMessage], viewType: ViewType): Behavior[ViewMessage] =
    start(parent, viewType)

  private def start(parent: ActorRef[ClientMessage], viewType: ViewType): Behavior[ViewMessage] =
    new Viewer(parent, ViewFactory(viewType)).working()
}

/**
 * Class of Viewer Actor.
 */
class Viewer(parent: ActorRef[ClientMessage], view: View) {

  def working(): Behavior[ViewMessage] = Behaviors.receive { (_, message) =>
    message match {
      case ShowBoard(board: Board) =>
        view.showBoard(board)
        Behaviors.same
      case ShowMessage(message: String) =>
        view.showMessage(message)
        Behaviors.same
      case ShowResult(result: Result) =>
        view.showResult(result)
        Behaviors.same
      case _ =>
        Behaviors.same
    }
  }
}
