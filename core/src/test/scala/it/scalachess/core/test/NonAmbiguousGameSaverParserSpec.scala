package it.scalachess.core.test

import it.scalachess.core.{ Draw, White, Win }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }
import it.scalachess.core.parser.NonAmbiguousGameSaver

class NonAmbiguousGameSaverParserSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  "The GameSaver parser " should "be able to create the game string with no ambiguity" in {
    val game = GameCreator.scholarGame
    val result: String = s"1.e4 e5\n" +
    s"2.Bc4 Nc6\n" +
    s"3.Qh5 Nf6\n" +
    s"4.Qxf7#\n" +
    s"1-0"
    game.gameStatus shouldEqual (Win(White))
    game.isKingInCheck shouldBe true
    NonAmbiguousGameSaver.convertAndFormat(game.moveHistory, Some(Win(White))) shouldEqual result
  }

  it should "be able to print a game with draw" in {
    val result: String = s"1.e3 a5\n" +
    s"2.Qh5 Ra6\n" +
    s"3.Qxa5 h5\n" +
    s"4.h4 Rah6\n" +
    s"5.Qxc7 f6\n" +
    s"6.Qxd7+ Kf7\n" +
    s"7.Qxb7 Qd3\n" +
    s"8.Qxb8 Qh7\n" +
    s"9.Qxc8 Kg6\n" +
    s"10.Qe6\n" +
    s"1/2"
    val drawGame = GameCreator.drawGame
    drawGame.gameStatus shouldEqual Draw
    NonAmbiguousGameSaver.convertAndFormat(drawGame.moveHistory, Some(Draw)) shouldEqual result
  }
}
