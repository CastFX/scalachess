package it.scalachess.core.pieces

import it.scalachess.core.Color

final case class Piece(color: Color, pieceType: PieceType) {

  lazy val symbol: String = pieceType.symbol(color)

}
