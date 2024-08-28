package games.wrg

import Ingredient.*
import simulators.*
import stateful.*

import cats.data.State
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
    import Sim_2_2.*
    "resolve Viscera-2" in {
      val deck = Vector(Card(3, Slime), Card(2, Viscera), Card(3, Mineral), Card(2, Flora), Card(3, Soil))
      val sim = for {
        _ <- flipOnce // flip Slime-3
        _ <- flipOnce // flip V-2 (and decide what to put on top, should be F-2
        _ <- flipOnce // flip F-2
      } yield ()

      val result = sim.runS(PlayerBoard(deck)).value
      result.cauldron.lastOption shouldBe Some(Card(2, Flora))
    }
    "resolve Slime-2 against a target and another Slime-2" in {
      val cauldron = Vector(Card(2, Flora), Card(2, Slime))
      val deck = Vector(Card(2, Slime))
      val result = (flipOnce *> flipOnce).runS(PlayerBoard(deck, cauldron)).value
      result.cauldron shouldBe Vector(Card(2, Flora, true)) ++ Card(2, Slime) * 2
    }
    "resolve Slime-2 with no good target" in {
      val cauldron = Vector()
      val deck = Vector(Card(2, Slime))
      val result = flipOnce.runS(PlayerBoard(deck, cauldron)).value
      result.cauldron shouldBe Card(2, Slime) * 1
      result.gold shouldBe 1 // default mode
    }
    "resolve Mineral-2 with no target" in {
      val cauldron = Vector()
      val deck = Vector(Card(2, Mineral))
      val result = flipOnce.runS(PlayerBoard(deck, cauldron)).value
      result.cauldron shouldBe Card(2, Mineral) * 1
      result.gold shouldBe 1 // default mode
    }
    "resolve Mineral-2 with a target" in {
      val cauldron = Vector(Card(1, Soil))
      val deck = Vector(Card(2, Mineral))
      val result = flipOnce.runS(PlayerBoard(deck, cauldron)).value
      result.cauldron shouldBe Cards(Card(1, Soil, true), Card(2, Mineral))
      result.gold shouldBe 0 // default mode
    }
    "calculate Slime-3" in {
      val cauldron = Cards(
        Card(1, Mineral),Card(3, Slime, true),Card(1, Slime),Card(2, Fungus),Card(2, Soil)
      )
      val deck = Cards.empty
//      val result = sell.runS(PlayerBoard(deck, cauldron)).value
//      result.exploded shouldBe false
//      result.gold shouldBe 7
//
//      val y = State.modify[PlayerBoard](b => b.copy(gold = 10)).runS(PlayerBoard(deck, cauldron)).value
//      println(s"y = ${y.format}")
//      y.gold shouldBe 10
//
//      val x = sellEffects(Card(3, Slime)).runS(PlayerBoard(deck, cauldron)).value
//      x.totalUncuredGrade shouldBe 6
//      x.cauldron.count(_.grade == 2) shouldBe 2
//      println(x.format)
//      println(x.gold) // how the hell is this happening, it prints 2
//      x.gold shouldBe 2
      sellEffects.get(Card(3, Slime, true)).isDefined shouldBe true
      val justSells = resolveSellEffects(sellEffects).runS(PlayerBoard(deck, cauldron)).value
      println(s"justSells = ${justSells.format}")
      justSells.gold shouldBe 2

    }
  }

}
