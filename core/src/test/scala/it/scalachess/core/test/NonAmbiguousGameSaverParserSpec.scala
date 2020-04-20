package it.scalachess.core.test

import it.scalachess.core.{ ChessGame, Draw, White, Win }
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }
import it.scalachess.core.parser.NonAmbiguousGameSaver

class NonAmbiguousGameSaverParserSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  "The GameSaver parser " should "be able to create the game string with no ambiguity" in {
    val game = GameCreator.scholarGame
    val result: String = s"1.e4 e5\n" +
    s"2.Bc4 Nc6\n" +
    s"3.Qh5 Nf6\n" +
    s"4.Qxf7#\n" +
    s"1-0\n"
    game.gameStatus shouldEqual Win(White)
    game.isKingInCheck shouldBe true
    NonAmbiguousGameSaver.convertAndFormat(game.moveHistory, Some(Win(White))) shouldEqual result
  }

  it should "be able to handle a game with draw" in {
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
    s"1/2\n"
    val drawGame = GameCreator.drawGame
    drawGame.gameStatus shouldEqual Draw
    NonAmbiguousGameSaver.convertAndFormat(drawGame.moveHistory, Some(Draw)) shouldEqual result
  }

  it should "be able to handle a game with ambiguity on enpassant on same row" in {
    val enpassantGame: (ChessGame, ChessGame) = GameCreator.enpassantWithAmbiguityGame
    val enpassantLeft                         = enpassantGame._1
    val resultLeft: String = s"1.d4 a6\n" +
    s"2.f4 b6\n" +
    s"3.d5 a5\n" +
    s"4.f5 e5\n" +
    s"5.dxe6\n"
    NonAmbiguousGameSaver.convertAndFormat(enpassantLeft.moveHistory, None) shouldEqual resultLeft
    val enpassantRight = enpassantGame._2
    val resultRight: String = s"1.d4 a6\n" +
    s"2.f4 b6\n" +
    s"3.d5 a5\n" +
    s"4.f5 e5\n" +
    s"5.fxe6\n"
    NonAmbiguousGameSaver.convertAndFormat(enpassantRight.moveHistory, None) shouldEqual resultRight
  }

  it should "be able to handle a game with ambiguity on capture on the same col" in {
    val captureWithRookGame: (ChessGame, ChessGame) = GameCreator.sameColCaptureAmbiguityGame
    val captureDown                                 = captureWithRookGame._1
    val resultDown: String = s"1.a4 h5\n" +
    s"2.h4 g5\n" +
    s"3.hxg5 b5\n" +
    s"4.axb5 f5\n" +
    s"5.Ra6 f4\n" +
    s"6.Rh6 f3\n" +
    s"7.R6xh5\n"
    NonAmbiguousGameSaver.convertAndFormat(captureDown.moveHistory, None) shouldEqual resultDown
    val captureUp = captureWithRookGame._2
    val resultUp: String = s"1.a4 h5\n" +
    s"2.h4 g5\n" +
    s"3.hxg5 b5\n" +
    s"4.axb5 f5\n" +
    s"5.Ra6 f4\n" +
    s"6.Rh6 f3\n" +
    s"7.R1xh5\n"
    NonAmbiguousGameSaver.convertAndFormat(captureUp.moveHistory, None) shouldEqual resultUp
  }

}
