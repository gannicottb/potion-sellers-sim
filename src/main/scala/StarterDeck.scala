package games.wrg

import Ingredient.*
import simulator.Stack

enum StarterDeck(val cards: Stack):
  case A
      extends StarterDeck(
        Vector(
          Card(1, Slime),
          Card(1, Mineral),
          Card(1, Viscera),
          Card(2, Flora),
          Card(2, Soil),
          Card(2, Fungus)
        )
      )
  case B
      extends StarterDeck(
        Vector(
          Card(1, Mineral),
          Card(1, Viscera),
          Card(1, Flora),
          Card(2, Soil),
          Card(2, Fungus),
          Card(2, Slime)
        )
      )
  case C
      extends StarterDeck(
        Vector(
          Card(1, Viscera),
          Card(1, Flora),
          Card(1, Soil),
          Card(2, Fungus),
          Card(2, Slime),
          Card(2, Mineral)
        )
      )
  case D
      extends StarterDeck(
        Vector(
          Card(1, Flora),
          Card(1, Soil),
          Card(1, Fungus),
          Card(2, Slime),
          Card(2, Mineral),
          Card(2, Viscera)
        )
      )
  case E
      extends StarterDeck(
        Vector(
          Card(1, Soil),
          Card(1, Fungus),
          Card(1, Slime),
          Card(2, Mineral),
          Card(2, Viscera),
          Card(2, Flora)
        )
      )
  case F
      extends StarterDeck(
        Vector(
          Card(1, Fungus),
          Card(1, Slime),
          Card(1, Mineral),
          Card(2, Viscera),
          Card(2, Flora),
          Card(2, Soil)
        )
      )
  case M
      extends StarterDeck(
        Vector(
          Card(1, Mineral),
          Card(1, Mineral),
          Card(1, Mineral),
          Card(3, Mineral)
        )
      )
  case V extends StarterDeck(A.cards :+ Card(3, Viscera))
  case S extends StarterDeck(A.cards :+ Card(3, Slime))
end StarterDeck
