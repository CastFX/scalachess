package it.scalachess.ai.test

import it.scalachess.ai.AI
import it.scalachess.core.{Black, White}
import it.scalachess.ai.test.specifics.LevelZero
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

final case class LevelZeroSpec() extends FlatSpec with Matchers with GivenWhenThen with LevelZero {

  private val level = 0
  private val whiteAI = AI(level, White)
  private val blackAI = AI(level, Black)

  "The level zero chess A.I." should behave like generateAMove(whiteAI, blackAI)
  it should behave like chessAICantBeUsedDuringCheckmate(whiteAI, blackAI)

}