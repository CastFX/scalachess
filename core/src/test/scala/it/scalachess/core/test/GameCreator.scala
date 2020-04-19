package it.scalachess.core.test

import it.scalachess.core.ChessGame
import org.scalatest.OptionValues

object GameCreator extends OptionValues {

  lazy val scholarGame: ChessGame = movesToGame(Seq("e4", "e5", "Bc4", "Nc6", "Qh5", "Nf6", "Qxf7#"))

  lazy val castlingQueenSideGame: ChessGame = movesToGame(
    Seq("Na3", "a6", "b3", "b6", "c3", "c6", "Qc2", "d6", "Bb2", "e6", "0-0-0"))

  lazy val castlingKingSideGame: ChessGame = movesToGame(
    Seq("a3", "Nh6", "b3", "g6", "c3", "f6", "d3", "Bg7", "e3", "0-0"))

  lazy val drawGame: ChessGame = movesToGame(
    Seq("e3", "a5", "Qh5", "Ra6", "Qxa5", "h5", "h4", "Rah6", "Qxc7", "f6")
    ++ Seq("Qxd7+", "Kf7", "Qxb7", "Qd3", "Qxb8", "Qh7", "Qxc8", "Kg6", "Qe6"))

  lazy val kasparovImmortal: ChessGame = movesToGame(
    Seq("e4", "d6", "d4", "Nf6", "Nc3", "g6", "Be3", "Bg7", "Qd2", "c6", "f3", "b5")
    ++ Seq("Nge2", "Nbd7", "Bh6", "Bxh6", "Qxh6", "Bb7", "a3", "e5", "0-0-0", "Qe7")
    ++ Seq("Kb1", "a6", "Nc1", "0-0-0", "Nb3", "exd4", "Rxd4", "c5", "Rd1", "Nb6")
    ++ Seq("g3", "Kb8", "Na5", "Ba8", "Bh3", "d5", "Qf4+", "Ka7", "Rhe1", "d4")
    ++ Seq("Nd5", "Nbxd5", "exd5", "Qd6", "Rxd4", "cxd4", "Re7+", "Kb6", "Qxd4+", "Kxa5")
    ++ Seq("b4+", "Ka4", "Qc3", "Qxd5", "Ra7", "Bb7", "Rxb7", "Qc4", "Qxf6", "Kxa3")
    ++ Seq("Qxa6+", "Kxb4", "c3+", "Kxc3", "Qa1+", "Kd2", "Qb2+", "Kd1", "Bf1", "Rd2")
    ++ Seq("Rd7", "Rxd7", "Bxc4", "bxc4", "Qxh8", "Rd3", "Qa8", "c3", "Qa4+", "Ke1", "f4", "f5", "Kc1", "Rd2", "Qa7"))

  def movesToGame(moves: Seq[String]): ChessGame =
    moves.foldLeft(ChessGame.standard()) { (game, move) =>
      game(move).toOption.value
    }
}
