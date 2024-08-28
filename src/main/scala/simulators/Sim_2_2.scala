package games.wrg
package simulators
import Ingredient.*

import cats.data.State
import cats.syntax.all.*
import stateful.{PlayerBoard, Simulator, Step}

object Sim_2_2 {
  import stateful.{addToCauldron, flip, resolveFlip}

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

  // FIXME: curing a card makes it miss lookup here
  // could have a string key like "Flora-2"
  // could refactor as a function that pattern matches
  // could use a tuple
  val flipEffects: Map[Card, Step] = Map(
    Card(2, Flora)  -> cureOneCard_(_.grade == 2),
    Card(2, Fungus) -> State.modify { _.modifyLimit(_ + 1) },
    Card(2, Soil)   -> cureOneCard_(_.grade == 3),
    Card(2, Mineral) -> cureOneCard(_.grade == 1).flatMap { curedOne =>
      if (curedOne) State.pure(())
      else State.modify { _.modifyGold(_ + 1) }
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
//        println(s"resolving as $c yields $profit")
        profit
      )
      bestOption.fold(
        State.modify[PlayerBoard](_.modifyGold(_ + 1))
      )(flipEffects)
    }
  )

  val sellEffects: Map[Card, Step] = Map(
    Card(3, Slime, true) -> State.modify { b =>
      b.modifyGold(_ + b.cauldron.count(_.grade == 2))
    },
    Card(3, Slime, false) -> State.modify { b =>
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
    if (b.exploded) {
      b.modifyGold(_ + cauldronWithoutHighestGrades(b.cauldron).size)
    } else {
      b.modifyGold(_ + b.cauldron.size)
    }
  }

  def resolveSellEffects(m: Map[Card, Step]): Step =
    for {
      board <- State.get[PlayerBoard]
      // apply each applicable sell effect
//      _ <- m.getOrElse(Card(3, Slime), State.pure(()))

//      _ <- Vector(State.pure(()), m.getOrElse(Card(3, Slime), State.pure(())), State.pure(()))
//        .fold(State.pure(()))(_ *> _)

      _ <- (if (board.exploded) cauldronWithoutHighestGrades(board.cauldron) else board.cauldron)
        .map { c =>
          println(s"checking card $c")
          val step = m.getOrElse(c, State.pure(()))
          println(s"applying $step")
          step
        }
        .fold(State.pure(()))(_ *> _)
    } yield ()

  // FIXME: Circular dependencies on flipOnce and flipEffects and Version
  // the existence of slime makes it recursive because we need the map of effects
  // to know how to resolve a flip, but we need to know how to resolve a flip in order to implement
  // some of the effects
  // So the definitions are codependent.

  val flipOnce: Step = flip *> resolveFlip(flipEffects) *> addToCauldron
  val sell: Step     = scorePotion *> resolveSellEffects(sellEffects)

  val v: Version =
    Version(flipEffects, sellEffects, flipOnce, sell, (cs: Cards) => PlayerBoard.apply(deck = cs))
  val simulator: Simulator = Simulator(v)
}
