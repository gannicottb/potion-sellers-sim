package games.wrg

import Ingredient.*

enum TestDeck(val cards: Cards) extends Deck {
  case M
    extends TestDeck(
      Vector(
        Card(1, Mineral),
        Card(1, Mineral),
        Card(2, Mineral),
        Card(3, Mineral)
      )
    )
  case V extends TestDeck(StarterDeck.A.cards :+ Card(3, Viscera))
  case S extends TestDeck(StarterDeck.A.cards :+ Card(3, Slime))
}