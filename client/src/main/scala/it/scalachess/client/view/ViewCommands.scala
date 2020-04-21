package it.scalachess.client.view

import it.scalachess.core.board.Board
import it.scalachess.core.logic.Result

object ViewCommands {

  /**
   * Trait for messages sent to the View.
   */
  trait ViewMessage

  trait ViewCommand                       extends ViewMessage
  case class ShowBoard(board: Board)      extends ViewCommand
  case class ShowMessage(message: String) extends ViewCommand
  case class ShowResult(result: Result)   extends ViewCommand
}
