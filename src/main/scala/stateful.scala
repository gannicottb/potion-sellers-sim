package games.wrg

import cats.data.{IndexedStateT, State}
import cats.syntax.all.*
import Ingredient.*
import Cards.*

import cats.effect.kernel.Sync
import cats.effect.std.Random
import cats.{Eval, Monad, Parallel, Show, Traverse}

object stateful {
  // A transition from one PlayerBoard to another
  type Step = State[PlayerBoard, Unit]

  // Wrap up the game state for one player
  case class PlayerBoard(
      deck: Cards,
      flipped: Option[Card],
      cauldron: Cards,
      limit: Int,
      gold: Int
  ) {
    def totalUncuredGrade: Int                  = cauldron.filterNot(_.cured).map(_.grade).sum
    def exploded: Boolean                       = totalUncuredGrade > limit
    def modifyGold(fn: Int => Int): PlayerBoard = copy(gold = fn(gold))
    def format: String =
      List(
        s"Cauldron[${cauldron.size}](${cauldron
            .map(c => s"${c.subtype}-${c.grade}${if (c.cured) "*" else ""}")
            .mkString(",")})",
        s"Deck[${deck.size}](${deck.map(c => s"${c.subtype}-${c.grade}").mkString(",")})",
        s"Gold: ${gold}, ${cauldron.filterNot(_.cured).map(_.grade).sum}/$limit"
      ).mkString(",")
  }
  object PlayerBoard {
    def apply(deck: Cards, cauldron: Cards = Vector.empty[Card], limit: Int = 6): PlayerBoard =
      PlayerBoard(deck, None, cauldron, limit, 0)
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
  def resolveFlip(m: Map[Card, Step]): Step = {
    for {
      b <- State.get[PlayerBoard]
      _ <- b.flipped.flatMap(m.get).getOrElse(State.pure(()))
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

  def moveCardToTopOfDeck(index: Int): Step = State.modify[PlayerBoard] { b =>
    b.copy(
      deck = b.deck.putCardOnTop(index)
    )
  }

  trait Player extends Product with Serializable {
    def willFlip(board: PlayerBoard, version: Version): Boolean
  }

  // This guy never learns
  case object Goblin extends Player {
    def willFlip(board: PlayerBoard, version: Version): Boolean = true
  }

  // This one does the math (I think)
  // Players need to know what version of the game they're playing so that flipOnce and sell will work
  case class EVCalc(verbose: Boolean = false) extends Player {
    override def willFlip(board: PlayerBoard, version: Version): Boolean = {
      import version.*
      // calculate expected value = sum(% chance of each outcome * reward)
      if(verbose) println(s"Given current board ${board.format}")
      val possibles = board.deck.zipWithIndex
        .map { case (card, i) =>
          // for each card, simulate flipping it and find out how much gold you gain/lose
          val predict = (moveCardToTopOfDeck(i) *> flipOnce *> sell).runS(board).value
          if (verbose) println(s"predicting that if I flip $card the result will be ${predict.format}")
          val diff = predict.gold - sell.runS(board).value.gold
          if (verbose)
            println(
              s"The potential improvement is $diff (${predict.gold} - ${sell.runS(board).value.gold})"
            )
          card -> diff
        }
        .groupMap(_._1)(_._2)
      if (verbose) println(s"Colinbot sees $possibles")
      val expectedValueOfFlipping = possibles.foldLeft(0.0) { case (sum, (k, v)) =>
        sum + (v(0) * (v.length / board.deck.length.toDouble))
      }
      if (verbose) {
        println(s"Colinbot says ev of flipping again is $expectedValueOfFlipping")
        if(expectedValueOfFlipping > 0) println(s"Colinbot will flip again") else println("Colinbot passes!")
      }
      expectedValueOfFlipping > 0
    }
  }

  case class Gambler(riskTolerance: Double) extends Player {
    def willFlip(board: PlayerBoard, version: Version): Boolean = {
      import version.*
      import board.*
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

  trait HasCards[A]:
    extension (a: A) def cards: Cards

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
          case PlayerBoard(Vector(), _, _, _, _) =>
            if (verbose) println(s"$player ran out of cards w/o exploding, scoring all")
            sell
          // didn't explode, have more cards, want to flip again
          case _ if player.willFlip(b, version) =>
            if (verbose) println(s"$player didn't explode, wants to go again")
            flipOnce *> flipOrPass
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
