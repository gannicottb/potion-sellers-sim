package games.wrg
package simulators

import simulator.*
import Ingredient.*

import cats.data.State
import cats.syntax.all.*

object Sim_2_1 {
  val flipEffects: Compendium = {
    case Card(2, Fungus, _) => State.modify { b =>
      b.copy(limit = b.limit + 1)
    }
    case Card(2, Slime, _) => State.get.flatMap { b =>
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
      bestOption.fold(State.pure(()))(flipEffects.apply)
    }
    case Card(2, Mineral, _) => State.modify { b =>
      // +1G
      b.copy(gold = b.gold + 1)
    }
    case Card(2, Viscera, _) => State.modify { b =>
      // currently V-2 is in the limbo zone
      // Separate the top 3 cards from the deck
      val (top3, deck) = b.deck.splitAt(3)
      // create each possible configuration of the deck (default ordering for bottomed cards)
      val possibleDecks = top3.permutations
        .distinctBy(_.headOption)
        .map {
          case top +: rest => (top +: deck) ++ rest
          case _ => Vector()
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
    case Card(2, Flora, _) => State.modify { b =>
      // cure another 2
      // I want some kind of mapWithBoolean for this
      val (_, newCauldron) = b.cauldron
        .foldLeft((false, Vector.empty[Card])) {
          case ((false, result), c) if !c.cured && c.grade == 2 =>
            (true, result :+ c.copy(cured = true))
          case ((other, result), c) => (other, result :+ c)
        }
      b.copy(
        cauldron = newCauldron
      )
    }
    case Card(2, Soil, _) => State.modify { b =>
      // cure a 3
      b.cauldron.zipWithIndex
        .find((c, i) => c.grade == 3 && !c.cured)
        .fold(
          b
        ) { case (card, idx) =>
          b.copy(
            cauldron = b.cauldron.updated(idx, card.copy(cured = true))
          )
        }
    }
  }
//  val flipEffects: Map[Card, Step] = Map(
//    Card(2, Flora) -> State.modify { b =>
//      // cure another 2
//      // I want some kind of mapWithBoolean for this
//      val (_, newCauldron) = b.cauldron
//        .foldLeft((false, Vector.empty[Card])) {
//          case ((false, result), c) if !c.cured && c.grade == 2 =>
//            (true, result :+ c.copy(cured = true))
//          case ((other, result), c) => (other, result :+ c)
//        }
//      b.copy(
//        cauldron = newCauldron
//      )
//    },
//    Card(2, Fungus) -> State.modify { b =>
//      b.copy(limit = b.limit + 1)
//    },
//    Card(2, Soil) -> State.modify { b =>
//      // cure a 3
//      b.cauldron.zipWithIndex
//        .find((c, i) => c.grade == 3 && !c.cured)
//        .fold(
//          b
//        ) { case (card, idx) =>
//          b.copy(
//            cauldron = b.cauldron.updated(idx, card.copy(cured = true))
//          )
//        }
//    },
//    Card(2, Mineral) -> State.modify { b =>
//      // +1G
//      b.copy(gold = b.gold + 1)
//    },
//    Card(2, Viscera) -> State.modify { b =>
//      // currently V-2 is in the limbo zone
//      // Separate the top 3 cards from the deck
//      val (top3, deck) = b.deck.splitAt(3)
//      // create each possible configuration of the deck (default ordering for bottomed cards)
//      val possibleDecks = top3.permutations
//        .distinctBy(_.headOption)
//        .map {
//          case top +: rest => (top +: deck) ++ rest
//          case _           => Vector()
//        }
//        .toList
//      // println(s"possible configurations are: ${possibleDecks}")
//      val currentGold = sell.runS(b).value.gold
//      // println(s"current G = $currentGold, cauldron = ${b.cauldron}")
//      val highestScoringDeck = possibleDecks.maxByOption { d =>
//        // have to not forget to add the Viscera itself to the cauldron for the hypothetical
//        val profit =
//          (addToCauldron *> flipOnce *> sell).runS(b.copy(deck = d)).value.gold - currentGold
//        // println(s"putting ${d.headOption} on top yields $profit")
//        profit
//      }
//      // println(s"${highestScoringDeck} yields highest G")
//      // the highest EV will be on top, the rest bottomed
//      b.copy(deck = highestScoringDeck.getOrElse(b.deck))
//    },
//    Card(2, Slime) -> State.get.flatMap { b =>
//      val possibles = b.cauldron.filter(c => flipEffects.contains(c) && c.subtype != Slime)
//      // hypothetically resolve each effect. resolveFlipEffects currently assumes the resolver is in `flipped`
//      val currentGold = sell.runS(b).value.gold
//      val bestOption = possibles.maxByOption(c =>
//        // provide a fake map that says Slime-2 has the effect of the card we're evaluating
//        val profit = (resolveFlip(Map(Card(2, Slime) -> flipEffects(c))) *> addToCauldron *> sell)
//          .runS(b)
//          .value
//          .gold - currentGold
//        // println(s"resolving as $c yields $profit")
//        profit
//      )
//      bestOption.fold(State.pure(()))(flipEffects)
//    }
//  )

//  val sellEffects: Map[Card, Step] = Map(
//    Card(3, Slime) -> State.modify { b =>
//      b.modifyGold(_ + b.cauldron.count(_.grade == 2))
//    },
//    Card(3, Mineral) -> State.modify { b =>
//      b.modifyGold(_ + (b.cauldron.count(_.subtype == Mineral) - 1) * 2)
//    },
//    Card(3, Soil) -> State.modify { b =>
//      b.modifyGold(_ + (b.cauldron.count(_.grade == 3) - 1) * 3)
//    },
//    Card(3, Flora) -> State.modify { b =>
//      b.modifyGold(_ + b.cauldron.count(_.cured) * 2)
//    },
//    Card(3, Viscera) -> State.modify { b =>
//      b.modifyGold(_ + b.cauldron.map(_.subtype).distinct.length * 2)
//    },
//    Card(3, Fungus) -> State.modify { b =>
//      b.modifyGold(_ + b.cauldron.map(_.grade).distinct.length * 2)
//    }
//  )
  val sellEffects: Compendium = {
    case Card(3, Slime, _) =>
      State.modify { b =>
        b.modifyGold(_ + b.cauldron.count(_.grade == 2))
      }
    case Card(3, Mineral, _) =>
      State.modify { b =>
        b.modifyGold(_ + (b.cauldron.count(_.subtype == Mineral) - 1) * 2)
      }
    case Card(3, Soil, _) =>
      State.modify { b =>
        b.modifyGold(_ + (b.cauldron.count(_.grade == 3) - 1) * 3)
      }
    case Card(3, Flora, _) =>
      State.modify { b =>
        b.modifyGold(_ + b.cauldron.count(_.cured) * 2)
      }
    case Card(3, Viscera, _) =>
      State.modify { b =>
        b.modifyGold(_ + b.cauldron.map(_.subtype).distinct.length * 2)
      }
    case Card(3, Fungus, _) =>
      State.modify { b =>
        b.modifyGold(_ + b.cauldron.map(_.grade).distinct.length * 2)
      }
  }

  val scorePotion: Step = State.modify { board =>
    board.modifyGold(_ + board.cauldron.size)
  }

  def resolveSellEffects(m: Compendium): Step =
    for {
      board <- State.get
      _     <- board.cauldron.map(c => m.getOrElse(c, State.pure(()))).fold(State.pure(()))(_ *> _)
    } yield ()

  // perform an entire flip
  val flipOnce: Step = flip *> resolveFlip(flipEffects) *> addToCauldron

  val sell: Step = State.get.flatMap {
    case b if b.exploded => scorePotion
    case _               => scorePotion *> resolveSellEffects(sellEffects)
  }

  val v = Version(flipEffects, sellEffects, flipOnce, sell, (cs: Cards) => PlayerBoard.apply(cs))
  val simulator: Simulator = Simulator(v)
}
