package it.scalachess.core.colors

sealed trait Color {
  val name: String
  val other: Color
}

final case object Black extends Color {
  val name         = "Black"
  val other: Color = White
}

final case object White extends Color {
  val name         = "White"
  val other: Color = Black
}
