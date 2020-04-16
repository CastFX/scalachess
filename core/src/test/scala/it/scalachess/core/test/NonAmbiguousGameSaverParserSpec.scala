package it.scalachess.core.test

import it.scalachess.core.{ White, Win }
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
    game.gameStatus should equal(Win(White))
    game.isKingInCheck shouldBe true
    println(NonAmbiguousGameSaver.convertAndFormat(game.moveHistory, Some(Win(White))))
    NonAmbiguousGameSaver.convertAndFormat(game.moveHistory, Some(Win(White))) shouldEqual result
  }
}
