package games.wrg

final case class Card(grade: Int, subtype: Ingredient, cured: Boolean = false) {
  def *(qty: Int): Vector[Card] = Vector.fill(qty)(this.copy())
}
