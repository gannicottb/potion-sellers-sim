package games.wrg

import simulators.Sim_2_1
import stateful.{EVCalc, PlayerBoard}
import Ingredient.*

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class PlayerSpec extends AnyWordSpec with Matchers {
  "EVCalc 2.1" should {
    import Sim_2_1.*
    "decide to flip if EV is positive" in {
      val cauldron = Vector(Card(1, Soil))
      val deck = Vector(Card(1, Slime))
      EVCalc(true).willFlip(PlayerBoard(deck, cauldron), v)
    }
    "decide to pass if EV is 0" in {
      val cauldron = Vector()
      val deck = Vector()
      EVCalc(true).willFlip(PlayerBoard(deck, cauldron), v) shouldBe false
    }
    "decide to pass if EV is negative" in {
      val cauldron = Vector(Card(3, Soil), Card(3, Soil))
      val deck = Vector(Card(1, Viscera))
      EVCalc(true).willFlip(PlayerBoard(deck, cauldron), v) shouldBe false
    }
    
  }

  "EVCalc 2.2" should {
    import Sim_2_1.*
    "decide to flip if EV is positive" in {
      val cauldron = Vector(Card(1, Soil))
      val deck = Vector(Card(1, Slime))
      EVCalc(true).willFlip(PlayerBoard(deck, cauldron), v)
    }
    "decide to pass if EV is 0" in {
      val cauldron = Vector()
      val deck = Vector()
      EVCalc(true).willFlip(PlayerBoard(deck, cauldron), v) shouldBe false
    }
    "decide to pass if EV is negative" in {
      val cauldron = Vector(Card(3, Soil), Card(3, Soil))
      val deck = Vector(Card(1, Viscera))
      EVCalc(true).willFlip(PlayerBoard(deck, cauldron), v) shouldBe false
    }
  }
}
