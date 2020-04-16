package it.scalachess.core.test

import it.scalachess.core.ChessGame
import org.scalatest.OptionValues

object GameCreator extends OptionValues {

  val scholarGame: ChessGame = {
    var game: ChessGame       = ChessGame.standard()
    val firstMoveWhitePawn    = "e4"
    val secondMoveBlackPawn   = "e5"
    val thirdMoveWhiteBishop  = "Bc4"
    val fourthMoveBlackKnight = "Nc6"
    val fifthMoveWhiteQueen   = "Qh5"
    val sixthMoveBlackKnight  = "Nf6"
    val seventhMoveWhiteQueen = "Qxf7#"
    game = game(firstMoveWhitePawn).toOption.value
    game = game(secondMoveBlackPawn).toOption.value
    game = game(thirdMoveWhiteBishop).toOption.value
    game = game(fourthMoveBlackKnight).toOption.value
    game = game(fifthMoveWhiteQueen).toOption.value
    game = game(sixthMoveBlackKnight).toOption.value
    game = game(seventhMoveWhiteQueen).toOption.value
    game
  }

  val drawGame: ChessGame = {
    var drawGame = ChessGame.standard()
    (Seq("e3", "a5", "Qh5", "Ra6", "Qxa5", "h5", "h4", "Rah6", "Qxc7", "f6")
    ++ Seq("Qxd7+", "Kf7", "Qxb7", "Qd3", "Qxb8", "Qh7", "Qxc8", "Kg6", "Qe6"))
      .foreach { move =>
        drawGame = drawGame(move).toOption.value
      }
    drawGame
  }

}
