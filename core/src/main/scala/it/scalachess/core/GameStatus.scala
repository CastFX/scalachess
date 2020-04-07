package it.scalachess.core

/**
 * The status of a chess game
 */
sealed trait GameStatus

/**
 * The ending result of a chess game
 */
sealed trait Result extends GameStatus

/**
 * The status of a game that is on going
 */
case object Ongoing extends GameStatus

/**
 * The status of a game that ended with a draw
 */
case object Draw extends Result

/**
 * The status of a game that ended with a win
 * @param player the player that won the game
 */
case class Win(player: Color) extends Result

/**
 * The status of a game that ended with a win by forfeit
 * @param player the other player that won the game by forfeit
 */
case class WinByForfeit(player: Color) extends Result
