package games.wrg
package simulators
import Ingredient.*

import cats.data.State
import cats.syntax.all.*
import simulator.*

object Sim_2_2 {
  val supply: Cards =
    for {
      ingr  <- Ingredient.values.toVector
      grade <- Vector(1, 2, 3)
      card  <- Card(grade, ingr) * 4
    } yield card

  // Try to cure a card and return whether a card was cured
  def cureOneCard(pred: Card => Boolean): State[PlayerBoard, Boolean] =
    State.get[PlayerBoard].transform { (b, u) =>
      val (curedOne, newCauldron) = b.cauldron
        .foldLeft((false, Vector.empty[Card])) {
          case ((false, result), c) if !c.cured && pred(c) =>
            (true, result :+ c.copy(cured = true))
          case ((other, result), c) => (other, result :+ c)
        }
      b.copy(cauldron = newCauldron) -> curedOne
    }
  // Cure a card and discard the result
  def cureOneCard_(pred: Card => Boolean): Step = cureOneCard(pred).map(_ => ())

  val flipComp: Compendium = {
    case Card(2, Fungus, _) =>
      State.modify {
        _.modifyLimit(_ + 1)
      }
    case Card(2, Slime, _) =>
      State.get.flatMap { b =>
        val possibles = b.cauldron.filter(c => flipComp.contains(c) && c.subtype != Slime)
        // hypothetically resolve each effect. resolveFlipEffects currently assumes the resolver is in `flipped`
        val currentGold = sell.runS(b).value.gold
        val bestOption = possibles.maxByOption(c =>
          // provide a fake map that says Slime-2 has the effect of the card we're evaluating
          val profit =
            (resolveFlip { case Card(2, Slime, _) => flipComp(c) } *> addToCauldron *> sell)
              .runS(b)
              .value
              .gold - currentGold
          //        println(s"resolving as $c yields $profit")
          profit
        )
        bestOption.fold(
          State.modify[PlayerBoard](_.modifyGold(_ + 1))
        )(flipComp.apply)
      }
    case Card(2, Mineral, _) =>
      cureOneCard(_.grade == 1).flatMap { curedOne =>
        if (curedOne) State.pure(())
        else
          State.modify {
            _.modifyGold(_ + 1)
          }
      }
    case Card(2, Viscera, _) =>
      State.modify { b =>
        // currently V-2 is in the limbo zone
        // Separate the top 3 cards from the deck
        val (top3, deck) = b.deck.splitAt(3)
        // create each possible configuration of the deck (default ordering for bottomed cards)
        val possibleDecks = top3.permutations
          .distinctBy(_.headOption)
          .map {
            case top +: rest => (top +: deck) ++ rest
            case _           => Vector()
          }
          .toList
        // println(s"possible configurations are: ${possibleDecks}")
        val currentGold = sell.runS(b).value.gold
        // println(s"current G = $currentGold, cauldron = ${b.cauldron}")
        val highestScoringDeck = possibleDecks.maxByOption { d =>
          // have to not forget to add the Viscera itself to the cauldron for the hypothetical
          val profit =
            (addToCauldron *> flipOnce *> sell).runS(b.copy(deck = d)).value.gold - currentGold
          // println(s"putting ${d.headOption} on top yields $profit")
          profit
        }
        // println(s"${highestScoringDeck} yields highest G")
        // the highest EV will be on top, the rest bottomed
        b.copy(deck = highestScoringDeck.getOrElse(b.deck))
      }
    case Card(2, Flora, _) => cureOneCard_(_.grade == 2)
    case Card(2, Soil, _)  => cureOneCard_(_.grade == 3)
  }

  val sellComp: Compendium = {
    case Card(3, Fungus, _) =>
      State.modify { b =>
        b.modifyGold(_ + b.cauldron.map(_.grade).distinct.size * 2)
      }
    case Card(3, Slime, _) =>
      State.modify { b =>
        b.modifyGold(_ + b.cauldron.count(_.grade == 2))
      }
    case Card(3, Mineral, _) =>
      State.modify { b =>
        b.modifyGold(_ + b.cauldron.count(_.subtype == Mineral) * 2)
      }
    case Card(3, Viscera, _) =>
      State.modify { b =>
        b.modifyGold(_ + b.cauldron.map(_.subtype).distinct.size)
      }
    case Card(3, Flora, _) =>
      State.modify { b =>
        b.modifyGold(_ + b.cauldron.count(_.cured) * 2)
      }
    case Card(3, Soil, _) =>
      State.modify { b =>
        b.modifyGold(_ + b.cauldron.count(_.grade == 3) * 2)
      }
  }

  def cauldronWithoutHighestGrades(cauldron: Cards): Cards = {
    val maxGrade = cauldron.map(_.grade).maxOption.getOrElse(0)
    cauldron.filter(_.grade < maxGrade)
  }

  val scorePotion: Step = State.modify { b =>
    if (b.exploded) {
      b.modifyGold(_ + cauldronWithoutHighestGrades(b.cauldron).size)
    } else {
      b.modifyGold(_ + b.cauldron.size)
    }
  }

  def resolveSellEffects(comp: Compendium): Step =
    for {
      board <- State.get[PlayerBoard]
      // apply each applicable sell effect
      _ <- (if (board.exploded) cauldronWithoutHighestGrades(board.cauldron) else board.cauldron)
        .map { c => comp.get(c).getOrElse(State.pure(())) }
        .fold(State.pure(()))(_ *> _)
    } yield ()

  // FIXME: Circular dependencies on flipOnce and flipEffects and Version
  // the existence of slime makes it recursive because we need the map of effects
  // to know how to resolve a flip, but we need to know how to resolve a flip in order to implement
  // some of the effects
  // So the definitions are codependent.

  // salt goes in between resolveFlip and addToCauldron
  val flipOnce: Step = flip *> resolveFlip(flipComp) *> addToCauldron
  val sell: Step     = scorePotion *> resolveSellEffects(sellComp)

  val v: Version =
    Version(flipComp, sellComp, flipOnce, sell, (cs: Cards) => PlayerBoard.apply(deck = cs))
  val simulator: Simulator = Simulator(v)
}
