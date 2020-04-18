package it.scalachess.ai.test

import it.scalachess.ai.AI
import it.scalachess.ai.test.specifics.{LevelOne, LevelTwo, LevelZero, LevelThree}
import it.scalachess.core.{Black, White}
import org.scalatest.{FlatSpec, GivenWhenThen, Matchers}

final case class LevelThreeSpec() extends FlatSpec with Matchers with GivenWhenThen with LevelZero with LevelOne with LevelTwo with LevelThree {

  private val level = 3
  private val whiteAI = AI(level, White)
  private val blackAI = AI(level, Black)

  "The level three chess A.I." should behave like generateAMove(whiteAI, blackAI)
  it should behave like generateSimpleCapture(whiteAI)
  it should behave like generateTheMostValuedCapture(whiteAI)
  // it should behave like willBeTrickedOnTheNextEnemyMove(whiteAI, blackAI) // this test will fail at level two
  it should behave like willNotBeTrickedOnTheNextEnemyMove(whiteAI)
  it should behave like generateLastFoolsMateMove(blackAI)
  it should behave like generateLastScholarsMateMove(whiteAI)
  // it should behave like willBeTrickedOnTheSecondNextEnemyMove(whiteAI, blackAI)

  it should behave like generateKnightMoveAtTheStart(whiteAI)

  it should behave like chessAICantBeUsedDuringCheckmate(whiteAI, blackAI)

}