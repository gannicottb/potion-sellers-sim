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
      if(exploded || overLimit) {
        this.copy(gold = this.gold + basePay)
      } else {
        val goldFrom3s = this.cauldron.filter(_.grade == 3).map(c =>
          compendium.get(c).fold(
            0
          )(fn =>
            fn(this).gold
          )
        ).sum
        this.copy(gold = this.gold + basePay + goldFrom3s)
      }
    }
  }

  trait Strategy extends Product with Serializable {
    def willFlip(boardState: BoardState): Boolean
  }

  case object VerySafe extends Strategy {
    def willFlip(boardState: BoardState): Boolean = {
      import boardState.*
      limit - cauldron.filterNot(_.cured).map(_.grade).sum >= deck
        .maxBy(_.grade)
        .grade
    }
  }

  case object Gambler extends Strategy {
    def willFlip(boardState: BoardState): Boolean = {
      import boardState.*
      val spaceRemaining = limit - cauldron.filterNot(_.cured).map(_.grade).sum
      // if the odds of flipping something that fits are 50%, we go for it
      val percentToNotExplode =
        deck.count(_.grade <= spaceRemaining).toDouble / deck.length
      percentToNotExplode >= .5
    }
  }

//  def calculateEV(boardState: BoardState)

  case class Colin(verbose: Boolean = false) extends Strategy {
    override def willFlip(boardState: BoardState): Boolean = {
      // calculate expected value = sum(% chance of each outcome * reward)
      val possibles = boardState.deck.zipWithIndex.map {
        case (card, i) =>
          val o = resolveFlip(boardState, i) // hypothetical
          val newGold = o.scored.gold
          val profit = newGold - boardState.scored.gold
          card -> profit
      }.groupMap(_._1)(_._2) // create a map of Card -> Vector[Int], so we can count % of duplicate cards
      if(verbose) println(s"Colinbot sees $possibles")
      val expectedValueOfFlipping = possibles.foldLeft(0.0){
          case (sum, (k, v)) => sum + (v(0) * (v.length / boardState.deck.length.toDouble))
        }
      if(verbose) println(s"Colinbot says ev of flipping again is $expectedValueOfFlipping")
      expectedValueOfFlipping > 0
    }
  }

  // TODO: Gambler should take a percentage
  // TODO: All strategies should(?) factor in the possibility of flipping a card that extends the sequence
  //        So if you're at 5 with a 2 in pot, a Flora-2 is actually safe to flip
  // TODO: In order to implement behavior for V-2 and S-2, need to answer this question:
  // outcome(cauldron, possibleCard): BoardState
  // basically the meat of the willFlip conditional that actually goes through the steps
  // factor that out and make it something we can run hypotheticals with

  // draw any card and get the rest of the deck
  def drawFrom(deck: Stack, index: Int): (Card, Stack) = {
    val (topN, rest) = deck.splitAt(index + 1) // index 0 -> top card (left side of vector)
    val (a, Vector(n)) = topN.splitAt(topN.length - 1) // get the extras
    n -> (a ++ rest)
  }

  extension (s: Stack)
    def draw(index: Int): (Card, Stack) = drawFrom(s, index)

  def resolveFlip(boardState: BoardState, index: Int = 0): BoardState = {
    // flip and update the cauldron and deck
    val (flipped, rest) = drawFrom(boardState.deck, index)
    val afterFlip = BoardState(boardState.cauldron :+ flipped, rest, boardState.limit)
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
      case BoardState(cauldron, _, limit, gold, false)
          if boardState.overLimit =>
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
        b.cauldron
          .zipWithIndex
          .filter((c, i) => c.grade < 3 && !c.cured && i < b.cauldron.size - 1)
          .maxBy((c, i) => c.grade)
      ).toOption
        .map((c, i) =>
          b.copy(
            cauldron = b.cauldron.updated(i, c.copy(cured = true))
          )
        )
        .getOrElse(b)
    },
    Card(2, Mineral) -> {(b: BoardState) => b.copy(gold = b.gold + 1)},
    Card(2, Soil) -> {(b: BoardState) =>
      b.cauldron.zipWithIndex.find((c, i) => c.grade == 3).fold(
        b
      ) {
        case (card, idx) =>
        b.copy(
          cauldron = b.cauldron.updated(idx, card.copy(cured = true))
        )
      }
    },
    Card(2, Fungus) -> { (b: BoardState) =>
      b.copy(limit = b.limit + 1)
    },
    Card(2, Viscera) -> identity, // Have to calculate EV of each of the next 3 cards
    Card(2, Slime) -> identity, // Pick highest EV flip to copy if able
    Card(3, Mineral) -> { (b: BoardState) =>
      b.copy(gold = (b.cauldron.count(_.subtype == Mineral) - 1) * 2)
    }
  )

  enum StarterDeck(val cards: Stack) {
    case A extends StarterDeck(
      Vector(
        Card(1, Slime),
        Card(1, Mineral),
        Card(1, Viscera),
        Card(2, Flora),
        Card(2, Soil),
        Card(2, Fungus)
      )
    )
    case B extends StarterDeck(
      Vector(
        Card(1, Mineral),
        Card(1, Viscera),
        Card(1, Flora),
        Card(2, Soil),
        Card(2, Fungus),
        Card(2, Slime)
      )
    )
    case C extends StarterDeck(Vector(
      Card(1, Viscera),
      Card(1, Flora),
      Card(1, Soil),
      Card(2, Fungus),
      Card(2, Slime),
      Card(2, Mineral)
    ))
    case D extends StarterDeck(Vector(
      Card(1, Flora),
      Card(1, Soil),
      Card(1, Fungus),
      Card(2, Slime),
      Card(2, Mineral),
      Card(2, Viscera)
    ))
    case E extends StarterDeck(Vector(
      Card(1, Soil),
      Card(1, Fungus),
      Card(1, Slime),
      Card(2, Mineral),
      Card(2, Viscera),
      Card(2, Flora)
    ))
    case F extends StarterDeck(Vector(
      Card(1, Fungus),
      Card(1, Slime),
      Card(1, Mineral),
      Card(2, Viscera),
      Card(2, Flora),
      Card(2, Soil)
    ))
    case M extends StarterDeck(Vector(
      Card(1, Mineral),
      Card(1, Mineral),
      Card(1, Mineral),
      Card(3, Mineral)
    ))
  }

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
  def run[F[_]: Monad](rnd: F[Random[F]], repetitions: Int, testCase: TestCase): F[List[BoardState]] = for {
    given Random[F] <- rnd
    a <- (0 until repetitions).toList.traverse(_ => runOnce[F](testCase))
  } yield a
}
