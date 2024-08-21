package games.wrg

import Ingredient.*

import cats.effect.std.Random
import cats.instances.list.*
import cats.syntax.all.*
import cats.{Functor, Monad}

import scala.annotation.tailrec
import scala.collection.immutable.Map
import scala.util.Try

object simulator {
  type Stack = Vector[Card]

  case class BoardState(
      cauldron: Stack,
      deck: Stack,
      limit: Int,
      gold: Int = 0,
      exploded: Boolean = false
  ) {
    def overLimit: Boolean = {
      cauldron.filterNot(_.cured).map(_.grade).sum > limit
    }
    def scored: BoardState = {
      val basePay = cauldron.size
      val sellBonuses =
        if (exploded || overLimit) 0
        else {
          this.cauldron
            .filter(_.grade == 3)
            .map(c =>
              compendium
                .get(c)
                .fold(
                  0
                )(fn => fn(this).gold // only works because the grade-3 compendium entries return only their value
                )
            )
            .sum
        }
      this.copy(gold = this.gold + basePay + sellBonuses)
    }
  }

  trait Strategy extends Product with Serializable {
    def willFlip(boardState: BoardState): Boolean
  }

  // Calculate likelihood of exploding, then flip if it's below tolerance
  case class Gambler(riskTolerance: Double) extends Strategy {
    def willFlip(boardState: BoardState): Boolean = {
      import boardState.*
      val percentToDie = deck.zipWithIndex
        .map { case (card, i) =>
          val o = resolveFlip(boardState, i) // hypothetical
          o.overLimit
        }
        .count(_ == true)
        .toDouble / deck.length // % chance the next flip causes explosion
      percentToDie <= riskTolerance
    }
  }

  //  def calculateEV(boardState: BoardState)

  // Calculate EV then flip if it's positive
  case class Colin(verbose: Boolean = false) extends Strategy {
    override def willFlip(boardState: BoardState): Boolean = {
      // calculate expected value = sum(% chance of each outcome * reward)
      val possibles = boardState.deck.zipWithIndex
        .map { case (card, i) =>
          val o       = resolveFlip(boardState, i) // hypothetical
          val newGold = o.scored.gold
          val profit  = newGold - boardState.scored.gold
          card -> profit
        }
        .groupMap(_._1)(
          _._2
        ) // create a map of Card -> Vector[Int], so we can count % of duplicate cards
      if (verbose) println(s"Colinbot sees $possibles")
      val expectedValueOfFlipping = possibles.foldLeft(0.0) { case (sum, (k, v)) =>
        sum + (v(0) * (v.length / boardState.deck.length.toDouble))
      }
      if (verbose) println(s"Colinbot says ev of flipping again is $expectedValueOfFlipping")
      expectedValueOfFlipping > 0
    }
  }

  // TODO: In order to implement behavior for V-2 and S-2, need to answer this question:
  // outcome(cauldron, possibleCard): BoardState
  // basically the meat of the willFlip conditional that actually goes through the steps
  // factor that out and make it something we can run hypotheticals with

  // draw any card and get the rest of the deck
  def drawFrom(deck: Stack, index: Int): (Card, Stack) = {
    val (topN, rest)   = deck.splitAt(index + 1)       // index 0 -> top card (left side of vector)
    val (a, Vector(n)) = topN.splitAt(topN.length - 1) // get the extras
    n -> (a ++ rest)
  }

  extension (s: Stack) def draw(index: Int): (Card, Stack) = drawFrom(s, index)

  def resolveFlip(boardState: BoardState, index: Int = 0): BoardState = {
    // flip and update the cauldron and deck
    val (flipped, rest) = drawFrom(boardState.deck, index)
    val afterFlip       = BoardState(boardState.cauldron :+ flipped, rest, boardState.limit)
    // resolve Flip effect
    if (flipped.grade == 2) {
      compendium
        .get(flipped)
        .fold(afterFlip)(_(afterFlip))
    } else afterFlip
  }

  @tailrec
  def flipOrPass(boardState: BoardState, strategy: Strategy): BoardState =
    boardState match {
      // flipped everything, so we're done
      case BoardState(cauldron, Vector(), limit, gold, _) => boardState.scored
      // went over the limit so we're done
      case BoardState(cauldron, _, limit, gold, false) if boardState.overLimit =>
        boardState.copy(exploded = true).scored
      // otherwise...
      case _ =>
        if (strategy.willFlip(boardState)) {
          // flip and update the cauldron and deck
          val afterResolution = resolveFlip(boardState)
          // recurse from there
          flipOrPass(
            afterResolution,
            strategy
          )
        } else boardState.scored // we're done
    }

  //  val s = State[Vector[Card], Vector[Card]](deck => (deck, Vector.empty[Card]))

  val compendium: Map[Card, BoardState => BoardState] = Map(
    Card(2, Flora) -> { (b: BoardState) =>
      Try(
        b.cauldron.zipWithIndex
          .filter((c, i) => c.grade < 3 && !c.cured && i < b.cauldron.size - 1)
          .maxBy((c, i) => c.grade)
      ).toOption
        .fold(b) { (c, i) =>
          b.copy(
            cauldron = b.cauldron.updated(i, c.copy(cured = true))
          )
        }
    },
    Card(2, Mineral) -> { (b: BoardState) => b.copy(gold = b.gold + 1) },
    Card(2, Soil) -> { (b: BoardState) =>
      b.cauldron.zipWithIndex
        .find((c, i) => c.grade == 3 && !c.cured)
        .fold(
          b
        ) { case (card, idx) =>
          b.copy(
            cauldron = b.cauldron.updated(idx, card.copy(cured = true))
          )
        }
//      val idx = b.cauldron.indexWhere(_.grade == 3)
//      if(idx > -1) b.copy(cauldron = b.cauldron.updated(idx, b.cauldron(idx).copy(cured = true)))
//      else b
    },
    Card(2, Fungus) -> { (b: BoardState) =>
      b.copy(limit = b.limit + 1)
    },
    Card(2, Viscera) -> identity, // Have to calculate EV of each of the next 3 cards
    Card(2, Slime)   -> identity, // Pick highest EV flip to copy if able
    // FIXME: these are actually weird in that they reset the gold value
    Card(3, Mineral) -> { (b: BoardState) =>
      b.copy(gold = (b.cauldron.count(_.subtype == Mineral) - 1) * 2)
    },
    Card(3, Viscera) -> { (b: BoardState) =>
      b.copy(gold = b.cauldron.map(_.subtype).distinct.length * 2)
    },
    Card(3, Slime) -> { (b: BoardState) =>
      b.copy(gold = b.cauldron.count(_.grade == 2) * 2)
    },
    Card(3, Fungus) -> { (b: BoardState) =>
      b.copy(gold = b.cauldron.map(_.grade).distinct.length * 2)
    },
    Card(3, Soil) -> { (b: BoardState) =>
      b.copy(gold = (b.cauldron.count(_.grade == 3) - 1) * 3)
    },
    Card(3, Flora) -> { (b: BoardState) =>
      b.copy(gold = b.cauldron.count(_.cured) * 2)
    }
  )

  val supply = for {
    grade      <- List(1, 2, 3)
    ingredient <- Ingredient.values.toList
    card       <- Card(grade, ingredient) * 4
  } yield card

  def runOnce[F[_]: Random: Functor](testCase: TestCase): F[BoardState] = {
    Random[F].shuffleVector(testCase.starterDeck.cards).map { deck =>
      flipOrPass(
        BoardState(
          cauldron = Vector.empty[Card],
          deck = deck,
          limit = 6
        ),
        testCase.strategy
      )
    }
  }

  final case class TestCase(strategy: Strategy, starterDeck: StarterDeck)
  def run[F[_]: Monad](
      rnd: F[Random[F]],
      repetitions: Int,
      testCase: TestCase
  ): F[List[BoardState]] =
    for {
      given Random[F] <- rnd
      a               <- (0 until repetitions).toList.traverse(_ => runOnce[F](testCase))
    } yield a
}
