package games.wrg

import Ingredient.*
import simulators.*
import stateful.*

import cats.syntax.all.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class CompendiumSpec extends AnyWordSpec with Matchers {
  "Version 2.1 Compendium" should {
    "resolve Viscera-2" in {
      val deck = Vector(Card(3, Slime), Card(2, Viscera), Card(2, Mineral), Card(2, Flora), Card(3, Soil))
      val sim = for {
        _ <- Sim_2_1.flipOnce // flip Slime-3
        _ <- Sim_2_1.flipOnce // flip V-2 (and decide what to put on top, should be F-2
        _ <- Sim_2_1.flipOnce // flip F-2
      } yield ()

      val result = sim.runS(PlayerBoard(deck)).value
      result.cauldron.lastOption shouldBe Some(Card(2, Flora))
    }
    "resolve Slime-2" in {
      val cauldron = Vector(Card(2, Flora), Card(2, Slime))
      val deck = Vector(Card(2, Slime))
      val result = (Sim_2_1.flipOnce *> Sim_2_1.flipOnce).runS(PlayerBoard(deck, cauldron)).value
      result.cauldron shouldBe Vector(Card(2, Flora, true)) ++ Card(2, Slime) * 2
    }
  }

  "Version 2.2 Compendium" should {
    "resolve Viscera-2" in {
      val deck = Vector(Card(3, Slime), Card(2, Viscera), Card(2, Mineral), Card(2, Flora), Card(3, Soil))
      val sim = for {
        _ <- Sim_2_2.flipOnce // flip Slime-3
        _ <- Sim_2_2.flipOnce // flip V-2 (and decide what to put on top, should be F-2
        _ <- Sim_2_2.flipOnce // flip F-2
      } yield ()

      val result = sim.runS(PlayerBoard(deck)).value
      result.cauldron.lastOption shouldBe Some(Card(2, Flora))
    }
    "resolve Slime-2 against a target and another Slime-2" in {
      val cauldron = Vector(Card(2, Flora), Card(2, Slime))
      val deck = Vector(Card(2, Slime))
      val result = (Sim_2_2.flipOnce *> Sim_2_2.flipOnce).runS(PlayerBoard(deck, cauldron)).value
      result.cauldron shouldBe Vector(Card(2, Flora, true)) ++ Card(2, Slime) * 2
    }
    "resolve Slime-2 with no good target" in {
      val cauldron = Vector()
      val deck = Vector(Card(2, Slime))
      val result = (Sim_2_2.flipOnce *> Sim_2_2.flipOnce).runS(PlayerBoard(deck, cauldron)).value
      result.cauldron shouldBe Card(2, Slime) * 1
      result.gold shouldBe 1 // default mode
    }
  }

}
