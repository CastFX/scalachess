package it.scalachess.core.test

import it.scalachess.core.White
import it.scalachess.core.board.Board
import it.scalachess.core.logic.MovesGenerator
import org.scalatest.{ FlatSpec, Inspectors, Matchers, OptionValues }

class MoveGenerationSpec extends FlatSpec with Matchers with Inspectors with OptionValues {

  "A standard board " should "boh" in {
    val generator = MovesGenerator(Board.defaultBoard(), White)
    generator()
  }

}
