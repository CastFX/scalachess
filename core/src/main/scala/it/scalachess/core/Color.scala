package it.scalachess.core.color

sealed trait Color {
  val name: String
  val other: Color
}

case object Black extends Color {
  val name         = "Black"
  val other: Color = White
}

case object White extends Color {
  val name         = "White"
  val other: Color = Black
}
