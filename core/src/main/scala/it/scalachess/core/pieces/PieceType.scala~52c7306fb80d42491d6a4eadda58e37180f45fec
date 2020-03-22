package it.scalachess.core.pieces

import it.scalachess.core.colors.{ Black, Color, White }

sealed trait PieceType {
  val name: String
  def symbol(of: Color): String
}

final case object King extends PieceType {
  val name = "King"
  def symbol(of: Color): String = of match {
    case White => "♔"
    case Black => "♚"
  }
}

final case object Queen extends PieceType {
  val name = "Queen"
  def symbol(of: Color): String = of match {
    case White => "♕"
    case Black => "♛"
  }
}

final case object Rook extends PieceType {
  val name = "Rook"
  def symbol(of: Color): String = of match {
    case White => "♖"
    case Black => "♜"
  }
}

final case object Bishop extends PieceType {
  val name = "Bishop"
  def symbol(of: Color): String = of match {
    case White => "♗"
    case Black => "♝"
  }
}
final case object Knight extends PieceType {
  val name = "Knight"
  def symbol(of: Color): String = of match {
    case White => "♘"
    case Black => "♞"
  }
}

final case object Pawn extends PieceType {
  val name = "Pawn"
  def symbol(of: Color): String = of match {
    case White => "♙"
    case Black => "♟"
  }
}
