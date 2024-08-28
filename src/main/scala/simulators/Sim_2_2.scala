package games.wrg
package simulators
import Ingredient.*

import cats.data.State
import cats.syntax.all.*
import stateful.{PlayerBoard, Simulator, Step}

object Sim_2_2 {
  import stateful.{addToCauldron, flip, resolveFlip}

  def cureOneCard(pred: Card => Boolean): Step = State.modify { b =>
    val (_, newCauldron) = b.cauldron
      .foldLeft((false, Vector.empty[Card])) {
        case ((false, result), c) if !c.cured && pred(c) =>
          (true, result :+ c.copy(cured = true))
        case ((other, result), c) => (other, result :+ c)
      }
    b.copy(cauldron = newCauldron)
  }

  val flipEffects: Map[Card, Step] = Map(
    Card(2, Flora) -> cureOneCard(_.grade == 2),
    Card(2, Fungus) -> State.modify { b =>
      b.copy(limit = b.limit + 1)
    },
    Card(2, Soil) -> cureOneCard(_.grade == 3),
    Card(2, Mineral) -> State.modify { b =>
      // cure a 1 if able, otherwise get gold
      b.cauldron.zipWithIndex
        .find((c, i) => c.grade == 1 && !c.cured)
        .fold(
          b.modifyGold(_ + 1)
        ) { case (card, idx) =>
          b.copy(
            cauldron = b.cauldron.updated(idx, card.copy(cured = true))
          )
        }
    },
    Card(2, Viscera) -> State.modify { b =>
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
    },
    Card(2, Slime) -> State.get.flatMap { b =>
      val possibles = b.cauldron.filter(c => flipEffects.contains(c) && c.subtype != Slime)
      // hypothetically resolve each effect. resolveFlipEffects currently assumes the resolver is in `flipped`
      val currentGold = sell.runS(b).value.gold
      val bestOption = possibles.maxByOption(c =>
        // provide a fake map that says Slime-2 has the effect of the card we're evaluating
        val profit = (resolveFlip(Map(Card(2, Slime) -> flipEffects(c))) *> addToCauldron *> sell)
          .runS(b)
          .value
          .gold - currentGold
        // println(s"resolving as $c yields $profit")
        profit
      )
      bestOption.fold(
        State.modify[PlayerBoard](_.modifyGold(_ + 1))
      )(flipEffects)
    }
  )

  val sellEffects: Map[Card, Step] = Map(
    Card(3, Slime) -> State.modify { b =>
      b.modifyGold(_ + b.cauldron.count(_.grade == 2))
    },
    Card(3, Mineral) -> State.modify { b =>
      b.modifyGold(_ + b.cauldron.count(_.subtype == Mineral) * 2)
    },
    Card(3, Soil) -> State.modify { b =>
      b.modifyGold(_ + b.cauldron.count(_.grade == 3) * 2)
    },
    Card(3, Flora) -> State.modify { b =>
      b.modifyGold(_ + b.cauldron.count(_.cured) * 2)
    },
    Card(3, Viscera) -> State.modify { b =>
      b.modifyGold(_ + b.cauldron.map(_.subtype).distinct.size)
    },
    Card(3, Fungus) -> State.modify { b =>
      b.modifyGold(_ + b.cauldron.map(_.grade).distinct.size * 2)
    }
  )

  def cauldronWithoutHighestGrades(cauldron: Cards): Cards = {
    val maxGrade = cauldron.map(_.grade).maxOption.getOrElse(0)
    cauldron.filter(_.grade < maxGrade)
  }

  val scorePotion: Step = State.modify { b =>
    if (b.exploded) b.modifyGold(_ + cauldronWithoutHighestGrades(b.cauldron).size)
    else b.modifyGold(_ + b.cauldron.size)
  }

  def resolveSellEffects(m: Map[Card, Step]): Step =
    for {
      board <- State.get
      _ <- (if (board.exploded) cauldronWithoutHighestGrades(board.cauldron) else board.cauldron)
        .map(c => m.getOrElse(c, State.pure(())))
        .fold(State.pure(()))(_ *> _)
    } yield ()

  // FIXME: Circular dependencies on flipOnce and flipEffects and Version
  // the existence of slime makes it recursive because we need the map of effects
  // to know how to resolve a flip, but we need to know how to resolve a flip in order to implement
  // some of the effects
  // So the definitions are codependent.

  val flipOnce: Step = flip *> resolveFlip(flipEffects) *> addToCauldron
  val sell: Step     = scorePotion *> resolveSellEffects(sellEffects)

  val v = Version(flipEffects, sellEffects, flipOnce, sell, (cs: Cards) => PlayerBoard.apply(deck = cs))
  val simulator: Simulator = Simulator(v)
}
