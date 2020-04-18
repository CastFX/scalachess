package it.scalachess.ai.test

import it.scalachess.ai.AI
import it.scalachess.ai.test.specifics.{LevelOne, LevelZero}
import it.scalachess.core.{Black, White}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

final case class LevelOneSpec() extends FlatSpec with Matchers with GivenWhenThen with LevelZero with LevelOne {

  private val level = 1
  private val whiteAI = AI(level, White)
  private val blackAI = AI(level, Black)

  "The level one chess A.I." should behave like generateAMove(whiteAI, blackAI)
  it should behave like generateSimpleCapture(whiteAI)
  it should behave like generateTheMostValuedCapture(whiteAI)
  it should behave like willBeTrickedOnTheNextEnemyMove(whiteAI, blackAI)

  it should behave like chessAICantBeUsedDuringCheckmate(whiteAI, blackAI)

}