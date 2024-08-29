package games.wrg

import cats.Show

final case class Card(grade: Int, subtype: Ingredient, cured: Boolean = false) {
  def *(qty: Int): Vector[Card] = Vector.fill(qty)(this.copy())
  val id: (Ingredient, Int) = subtype -> grade
}
object Card {
  given Show[Card] = Show.show(c => s"${c.subtype}-${c.grade}${if (c.cured) "*" else ""}")
}
