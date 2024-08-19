package games.wrg

trait Deck {
  def cards: Vector[Card]
  
  def draw(n: Int): (Vector[Card], Deck) = 
    cards.take(n) -> new Deck { def cards: Vector[Card] = cards.drop(n)}
    
  
}
  

