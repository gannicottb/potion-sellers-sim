package games.wrg

import Ingredient.*

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CardsSpec extends AnyWordSpec with Matchers {
  "Cards" should {
    "accept mixed input for a list" in {
      Cards(
        Card(1, Slime) * 2, Card(1, Viscera), Card(2, Flora) * 4
      ).size shouldBe 7
    }
  }

}
