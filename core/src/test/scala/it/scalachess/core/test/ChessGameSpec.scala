package it.scalachess.core.test

import it.scalachess.core.{ Black, ChessGame, Draw, Ongoing, White, Win, WinByForfeit }
import it.scalachess.core.board.{ Board, Position }
import it.scalachess.core.logic.moves.{ FullMove, ValidSimpleMove }
import it.scalachess.core.pieces.{ King, Knight, Pawn, Piece, Rook }
import org.scalatest.{ FlatSpec, Matchers, OptionValues }
import it.scalachess.core.test.ChessGameFailureMatcher.generateFailure

class ChessGameSpec extends FlatSpec with Matchers with OptionValues {
  val standardGame: ChessGame = ChessGame.standard()

  "A standard ChessGame" should "have a non-empty board" in {
    standardGame.board.pieces shouldBe 'nonEmpty
  }

  it should "have a starting white player at turn 0" in {
    standardGame.player should equal(White)
    standardGame.turn shouldBe 0
  }

  it should "not accept a move with an illegal format" in {
    standardGame("aa2 a3") should generateFailure
  }

  it should "not allows player to move an opponent piece" in {
    standardGame("a6") should generateFailure
  }

  it should "not allows a player captures his own pieces" in {
    standardGame("Qxd2") should generateFailure
  }

  it should "not allows specific pieces pass through other pieces" in {
    val invalidWhiteMoves = Seq("Qd3", "Qd4", "Qh5", "Qa4", "Bh6", "Ba3", "Ra6", "Bh3", "Ba6", "Rh6")
    invalidWhiteMoves.foreach(ChessGame.standard()(_) should generateFailure)

    val blackMovesGame    = GameCreator.movesToGame(Seq("a3"))
    val invalidBlackMoves = Seq("Ra3", "Ba6", "Bh3", "Qa5", "Qd5", "Qh4", "Ba3", "Bh6", "Rh6")
    invalidBlackMoves.foreach(blackMovesGame(_) should generateFailure)
  }

  it should "apply a move correctly" in {
    GameCreator.movesToGame(Seq("a4")).board.pieceAtPosition(Position(1, 4)).value shouldEqual Piece(White, Pawn)
  }

  it should "must alternate turns between players" in {
    val whiteMoved = GameCreator.movesToGame(Seq("a3"))
    whiteMoved.turn shouldBe 1
    whiteMoved.player shouldBe Black
    val blackMoved = whiteMoved("a6").toOption.value
    blackMoved.turn shouldBe 2
    blackMoved.player shouldBe White
  }

  "Build a game where the king " should "not being able do some move because it's under check" in {
    val game = GameCreator.movesToGame(Seq("e4", "e5", "Bd3", "d5", "f4", "Qh4+"))
    game.isKingInCheck shouldBe true
    game.gameStatus shouldBe Ongoing
    game("Kf2") should generateFailure
    game("Ke2").isSuccess shouldBe true
    game("Kf1").toOption.value.isKingInCheck shouldBe false
  }

  "Kasparov immortal" should "be played correctly" in {
    GameCreator.kasparovImmortal.gameStatus shouldBe Ongoing
    val blackForfeits = GameCreator.kasparovImmortal.end(WinByForfeit(White))
    blackForfeits.gameStatus shouldBe WinByForfeit(White)
  }

  /*
   * FOOL'S MATE
   * */
  "Build a Fool's Check Mate in which the game " should " end in 4 turn," in {
    val game = GameCreator.movesToGame(Seq("f3", "e6", "g4"))
    game.isKingInCheck shouldBe false
    val foolsMate = game("Qh4#").toOption.value
    foolsMate.isKingInCheck shouldBe true
    foolsMate.gameStatus should equal(Win(Black))
  }

  /*
   * SCHOLAR'S MATE
   * */
  "Build a Scholar's Check Mate in which the game " should " end in 7 turn," in {
    GameCreator.scholarGame.gameStatus should equal(Win(White))
    GameCreator.scholarGame.isKingInCheck shouldBe true
  }

  "Build a game where the white player" should "be able to use a QueenSide Castling" in {
    GameCreator.castlingQueenSideGame.board.pieceAtPosition(Position(3, 1)).value shouldBe Piece(White, King)
    GameCreator.castlingQueenSideGame.board.pieceAtPosition(Position(4, 1)).value shouldBe Piece(White, Rook)
  }

  "Build a game where the black player" should "be able to use a KingSide Castling" in {
    GameCreator.castlingKingSideGame.board.pieceAtPosition(Position(7, 8)).value shouldBe Piece(Black, King)
    GameCreator.castlingKingSideGame.board.pieceAtPosition(Position(6, 8)).value shouldBe Piece(Black, Rook)
  }

  "At the start of a game, a castling" should "not be allowed" in {
    val game: ChessGame = ChessGame.standard()
    game("0-0-0").toOption.isDefined shouldBe false
    game("0-0").toOption.isDefined shouldBe false
  }

  "A correct move history" should "be created during a game" in {
    val validMoves = Seq(
      ValidSimpleMove(Position(2, 1), Position(1, 3), Knight, White, None),
      ValidSimpleMove(Position(2, 7), Position(2, 6), Pawn, Black, None),
      ValidSimpleMove(Position(4, 2), Position(4, 3), Pawn, White, None),
      ValidSimpleMove(Position(4, 7), Position(4, 6), Pawn, Black, None),
      ValidSimpleMove(Position(7, 2), Position(7, 3), Pawn, White, None)
    )

    val history = validMoves.map { v =>
      val movesBefore = validMoves.span(_ != v)._1
      val boardAfter =
        movesBefore.foldLeft(Board.defaultBoard())((board, move) => board(move.boardChanges))(v.boardChanges)
      FullMove(v, resultsInCheck = false, resultsInCheckmate = false, boardAfter)
    }

    val game = GameCreator.movesToGame(Seq("Na3", "b6", "d3", "d6", "g3"))
    game.moveHistory shouldEqual history
  }

  "A Pawn on Position h4" should "be able to capture on g5" in {
    val game = GameCreator.movesToGame(Seq("h4", "g5", "hxg5"))
    game.board.pieces(Position(7, 5)).color shouldBe White
  }

  "A stalemate" should "result in a draw" in {
    GameCreator.drawGame.gameStatus shouldBe Draw
  }
}
