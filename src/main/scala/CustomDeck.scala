package games.wrg

import Ingredient.*

enum CustomDeck(val cards: Cards) extends Deck {
  case M
    extends CustomDeck(
      Vector(
        Card(1, Mineral),
        Card(1, Mineral),
        Card(2, Mineral),
        Card(3, Mineral)
      )
    )
  // Approximation of a game winning deck
  case V extends CustomDeck(
    Cards(
      Card(3, Viscera),
      Card(2, Viscera),
      Card(2, Flora) * 2,
      Card(2, Slime),
      Card(2, Mineral),
      Card(2, Soil),
      Card(1, Soil),
      Card(1, Fungus)
    )
  )
  case S extends CustomDeck(StarterDeck.A.cards :+ Card(3, Slime))
  case S2 extends CustomDeck(
    Cards(
      Card(2, Slime) * 2,
      Card(1, Flora) * 4
    )
  )

  case VV extends CustomDeck(
    Cards(
      Card(2, Slime) * 2,
      Card(2, Flora) * 4,
      Card(3, Slime)
    )
  )

  case SM extends CustomDeck(
    Cards(
      Card(2, Fungus) * 2,
      Card(2, Flora),
      Card(2, Viscera),
      Card(2, Slime),
      Card(1, Soil),
      Card(1, Mineral),
      Card(1, Mineral),
      Card(3, Slime),
      Card(3, Viscera)
    )
  )
}