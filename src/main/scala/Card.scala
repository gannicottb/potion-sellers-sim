package games.wrg

final case class Card(grade: Int, subtype: Ingredient, cured: Boolean = false) {
  def *(qty: Int): List[Card] = List.fill(qty)(this.copy())
}
