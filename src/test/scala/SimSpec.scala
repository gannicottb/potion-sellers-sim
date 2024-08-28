package games.wrg

import simulators.{Sim_2_1, Sim_2_2}

import games.wrg.simulators.simulator.*
import Ingredient.*

import cats.data.State
import cats.syntax.all.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SimSpec extends AnyWordSpec with Matchers {
  val initial = PlayerBoard(StarterDeck.A.cards)
  "moveCardToTop" should {
    "work for top card" in {
      val moved = moveCardToTopOfDeck(0).runS(initial).value
      moved.deck shouldBe initial.deck
    }
    "work for next card" in {
      val moved = moveCardToTopOfDeck(1).runS(initial).value
      moved.deck(0) shouldBe initial.deck(1)
    }
  }

  "flip" should {
    "work" in {
      val after = flip.runS(initial).value
      after.deck shouldBe initial.deck.tail
      after.flipped shouldBe initial.deck.headOption
    }
  }

  "resolveFlip" should {
    "work" in {
      val after: PlayerBoard =
        (flip *> resolveFlip{
          case c if c == initial.deck.head => State.modify[PlayerBoard] {
            _.copy(gold = 100)
          }
        })
          .runS(initial)
          .value
      after.gold shouldBe 100
    }
  }

  "addToCauldron" should {
    "work" in {
      val after = (flip *> resolveFlip(Compendium.empty) *> addToCauldron).runS(initial).value
      after.cauldron shouldBe initial.deck.take(1)
      after.flipped shouldBe None
      after.deck shouldBe initial.deck.drop(1)
    }
  }

  "2.1 sell" should {
    "work" in {
      val after = (flip *> resolveFlip(Compendium.empty) *> addToCauldron *> Sim_2_1.sell).runS(initial).value
      after.gold shouldBe 1
    }
  }

  "2.2 sell" should {
    import games.wrg.simulators.Sim_2_2.*
    "ignore highest grades when exploded" in {
      val deck = Vector(Card(1, Viscera))
      val cauldron = Card(3, Soil) * 2

      val sim = for {
        _ <- flipOnce
        _ <- sell
      } yield ()

      val result = sim.runS(PlayerBoard(deck, cauldron)).value
      result.exploded shouldBe true
      result.gold shouldBe 1
    }
    "work normally when not exploded" in {
      val deck = Vector()
      val cauldron = Card(3, Soil) * 2

      val result = sell.runS(PlayerBoard(deck, cauldron)).value
      result.exploded shouldBe false
      result.gold shouldBe 10
    }

  }

  "2.2 SaltOrNot" should {
    import simulators.Sim_2_2.*
    "work" in {
      val deck = Vector()
      val cauldron = Cards(Card(2, Slime) * 3, Card(3, Slime))
      val result = (useSaltOrNot(EVCalc(), v) *> sell).runS(PlayerBoard(deck, cauldron)).value
      result.saltAvailable shouldBe false
      result.gold shouldBe 4
    }
  }

}
