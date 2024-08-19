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
    def scored: BoardState = {
      val basePay = cauldron.size
      if(exploded) {
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

  trait Strategy {
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

  @tailrec
  def flipOrPass(boardState: BoardState, strategy: Strategy): BoardState =
    boardState match {
      // flipped everything, so we're done
      case BoardState(cauldron, Vector(), limit, gold, _) => boardState.scored
      // went over the limit so we're done
      case BoardState(cauldron, _, limit, gold, false)
          if cauldron.filterNot(_.cured).map(_.grade).sum > limit =>
        boardState.copy(exploded = true).scored
      // otherwise...
      case _ =>
        if (strategy.willFlip(boardState)) {
          // flip and update the cauldron and deck
          val (Vector(flipped), rest) = boardState.deck.splitAt(1)
          val afterFlip = BoardState(boardState.cauldron :+ flipped, rest, boardState.limit)
          // resolve Flip effect
          val afterResolution = if(flipped.grade == 2) {
            compendium
              .get(flipped)
              .fold(afterFlip)(fn => fn(afterFlip))
          } else afterFlip

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

  enum StarterDeck(val cards: Stack){
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
    case MineralDream extends StarterDeck(Vector(
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
    a <- List.fill(repetitions)(()).traverse(_ => runOnce[F](testCase))
  } yield a
}
