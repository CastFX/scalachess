package it.scalachess.client.view

sealed trait ViewType
case object CLI extends ViewType

/**
 * Factory to create a view.
 */
object ViewFactory {
  def apply(viewType: ViewType): View =
    viewType match {
      case CLI => CliView
      case _   => CliView
    }
}
