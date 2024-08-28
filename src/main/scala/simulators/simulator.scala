package games.wrg
package simulators

import Cards.*
import Cards.given

import cats.data.{IndexedStateT, State}
import cats.effect.kernel.Sync
import cats.effect.std.Random
import cats.syntax.all.*
import cats.{Eval, Monad, Show}

import scala.util.Try

object simulator {
  // A transition from one PlayerBoard to another
  type Step = State[PlayerBoard, Unit]

  // Wrap up the game state for one player
  case class PlayerBoard(
      deck: Cards,
      flipped: Option[Card],
      cauldron: Cards,
      limit: Int,
      gold: Int,
      saltAvailable: Boolean
  ) {
    def totalUncuredGrade: Int                   = cauldron.filterNot(_.cured).map(_.grade).sum
    def exploded: Boolean                        = totalUncuredGrade > limit
    def modifyGold(fn: Int => Int): PlayerBoard  = copy(gold = fn(gold))
    def modifyLimit(fn: Int => Int): PlayerBoard = copy(limit = fn(limit))
    def format: String =
      List(
        s"Cauldron${cauldron.show}",
        s"Deck${deck.show}",
        s"Gold: $gold, ${cauldron.filterNot(_.cured).map(_.grade).sum}/$limit",
        s"Salt: ${if(saltAvailable)"Available" else "Used"}"
      ).mkString(", ")
  }
  object PlayerBoard {
    def apply(deck: Cards, cauldron: Cards = Vector.empty[Card], limit: Int = 6): PlayerBoard =
      PlayerBoard(deck, None, cauldron, limit, 0, true)

    given Show[PlayerBoard] = Show.show(_.format)
  }

  trait Compendium {
    def get(card: Card): Option[Step] = Try(apply(card)).toOption
    def getOrElse(card: Card, default: Step): Step = get(card).getOrElse(default)
    def contains(card: Card): Boolean = get(card).isDefined

    def apply(card: Card): Step
  }
  object Compendium {
    def empty: Compendium = (card: Card) => {
      throw new MatchError("empty") // a little groady
    }
  }

// Go from deck to flipped
  val flip: Step = State.modify { board =>
    board.deck match {
      case (next: Card) +: (rest: Cards) =>
        board.copy(
          flipped = Some(next),
          deck = rest
        )
      case _ => board
    }
  }
  // resolve the effect (if any) of the flipped card
  def resolveFlip(comp: Compendium): Step = {
    for {
      b <- State.get[PlayerBoard]
      _ <- b.flipped.flatMap(comp.get).getOrElse(State.pure(()))
    } yield ()
  }

  // move the flipped card to the cauldron
  val addToCauldron: Step = State.modify { board =>
    board.flipped.fold(
      board
    ) { c =>
      board.copy(
        flipped = None,
        cauldron = board.cauldron :+ c
      )
    }
  }

  // Starting to think Player should/could be part of PlayerBoard
  // would make this much simpler. Version too.
  def useSaltOrNot(player: Player, version: Version): Step =
    State.get[PlayerBoard].flatMap { board =>
      if(player.willSalt(board, version)) useSalt else State.pure(())
    }

  // No op if salt not available
  val useSalt: Step = State.modify { board =>
    if(board.saltAvailable) board.copy(
      gold = board.gold - 3,
      cauldron = board.cauldron.updated(board.cauldron.size - 1, board.cauldron.last.copy(cured = true)),
      saltAvailable = false
    ) else board
  }

  def moveCardToTopOfDeck(index: Int): Step = State.modify[PlayerBoard] { b =>
    b.copy(
      deck = b.deck.putCardOnTop(index)
    )
  }

  trait Player extends Product with Serializable {
    def willFlip(board: PlayerBoard, version: Version): Boolean
    def willSalt(board: PlayerBoard, version: Version): Boolean = {
      import version.*
      // calculate EV of using Salt
      // somehow
      // case 1: the card you flipped increases your payout by more than 3
      // case 2: there is a juicy card still in your deck that you could flip if you salt now
      // TODO: relying on cauldron order is a little sketchy, can we do inject this while the card is still in
      //          `flipped`?
      val ifSalt = (useSalt *> sell).runS(board).value
      val ifNoSalt = sell.runS(board).value
      val diff = ifSalt.gold - ifNoSalt.gold
      board.saltAvailable && diff > 0
    }
  }

  // This one does the math (I think)
  // Currently doesn't value curing cards
  // Players need to know what version of the game they're playing so that flipOnce and sell will work
  case class EVCalc(verbose: Boolean = false) extends Player {
    override def willFlip(board: PlayerBoard, version: Version): Boolean = {
      import version.*
      // calculate expected value = sum(% chance of each outcome * reward)
      if (verbose) println(s"Given current board ${board.format}")
      val possibles = board.deck.zipWithIndex
        .map { case (card, i) =>
          // for each card, simulate flipping it and find out how much gold you gain/lose
          val predict = (moveCardToTopOfDeck(i) *> flipOnce *> sell).runS(board).value
          if (verbose)
            println(s"$i> If I flip $card the result will be ${predict.format}")
          val diff = predict.gold - sell.runS(board).value.gold
          if (verbose)
            println(
              s"The potential gold change is ${if (diff > 0) "+" else ""}$diff"
            )
          card -> diff
        }
        .groupMap(_._1)(_._2)
//      if (verbose) println(s"Colinbot sees $possibles")
      val expectedValueOfFlipping = possibles.foldLeft(0.0) { case (sum, (k, v)) =>
        sum + (v(0) * (v.length / board.deck.length.toDouble))
      }
      if (verbose) {
        println(s"Colinbot says ev of flipping again is $expectedValueOfFlipping")
        if (expectedValueOfFlipping > 0) println(s"Colinbot will flip again")
        else println("Colinbot passes!")
      }
      expectedValueOfFlipping > 0
    }
  }

  case class Gambler(riskTolerance: Double) extends Player {
    def willFlip(board: PlayerBoard, version: Version): Boolean = {
      import board.*
      import version.*
      val percentToDie = deck.zipWithIndex
        .map { case (card, i) =>
          val predict = (moveCardToTopOfDeck(i) *> flipOnce *> sell).runS(board).value
          predict.exploded
        }
        .count(_ == true)
        .toDouble / deck.length // % chance the next flip causes explosion
      percentToDie <= riskTolerance
    }
  }

  case class VibesBased(percentToFlip: Double) extends Player {
    override def willFlip(board: PlayerBoard, version: Version): Boolean = {
      // always flip until there's a chance of exploding, then only flip some of the time
      if(board.limit - board.totalUncuredGrade <= board.deck.maxBy(_.grade).grade){
        // randomly generate a double from 0 to 1.0
        scala.util.Random.nextDouble() <= percentToFlip
      } else true
    }
  }

  case class LabeledDeck(label: String, cards: Cards)
  object LabeledDeck {
    def apply(d: Deck): LabeledDeck = LabeledDeck(Show[Deck].show(d), d.cards)
  }
  case class SimCase(player: Player, startingDeck: LabeledDeck)

  case class Simulator(
      version: Version
  ) {
    import version.*
    def brew(player: Player, verbose: Boolean = false): Step = {
      def flipOrPass: Step = for {
        b <- State.get[PlayerBoard]
        _ = if (verbose) println(s"$player board: ${b.format}")
        _ <- b match {
          // exploded
          case _ if b.exploded =>
            if (verbose) println(s"$player exploded! scoring # of cards")
            sell
          // didn't explode, ran out of cards
          case PlayerBoard(Vector(), _, _, _, _, _) =>
            if (verbose) println(s"$player ran out of cards w/o exploding, scoring all")
            sell
          // didn't explode, have more cards, want to flip again
          case _ if player.willFlip(b, version) =>
            if (verbose) println(s"$player didn't explode, wants to go again")
            flipOnce *> useSaltOrNot(player, version) *> flipOrPass
          // didn't explode, have more cards, don't want to flip
          case _ =>
            if (verbose) println(s"$player didn't explode, wants to pass")
            sell
        }
      } yield ()

      flipOrPass
    }

    def runPermutations(testCase: SimCase): List[PlayerBoard] = {
      testCase.startingDeck.cards.permutations
        .map(d => brew(testCase.player).runS(initBoard(d)).value)
        .toList
    }

    def runPermutationsPar[F[_]: Sync](testCase: SimCase): F[List[PlayerBoard]] =
      testCase.startingDeck.cards.permutations.toList.traverse(d =>
        Sync[F].delay(brew(testCase.player).runS(initBoard(d)).value)
      )

    def runShuffled[F[_]: Monad](
        rnd: F[Random[F]],
        repetitions: Int,
        testCase: SimCase
    ): F[List[PlayerBoard]] =
      for {
        given Random[F] <- rnd
        a <- (0 until repetitions).toList.traverse { _ =>
          Random[F]
            .shuffleVector(testCase.startingDeck.cards)
            .map(shuffled => brew(testCase.player).runS(initBoard(shuffled)).value)
        }
      } yield a
  }
}
