package games.wrg

import stateful.HasCards

import cats.Show

trait Deck {
  def cards: Vector[Card]
  
  def draw(n: Int): (Vector[Card], Deck) = 
    cards.take(n) -> new Deck { def cards: Vector[Card] = cards.drop(n)}
}
object Deck {
  given Show[Deck] with
    override def show(t: Deck): String = t.toString
}
  

