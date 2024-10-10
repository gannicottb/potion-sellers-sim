package games.wrg

import simulators.simulator.{Compendium, PlayerBoard, Step}

import cats.{Id, ~>}
import cats.data.{IndexedReaderWriterStateT, Nested, Reader, ReaderT, State, StateT}
import cats.syntax.all.*

case class Version(
    flipEffects: Compendium,
    sellEffects: Compendium,
    flipOnce: Step,
    sell: Step,
    initBoard: Cards => PlayerBoard
)
// EXPERIMENTS AHEAD, WEAR A HELMET

trait Player {
  def willFlip(v: VersionTrait): BoardState[Boolean]
  def willSalt(v: VersionTrait, card: Card): BoardState[Boolean] = State.inspect { board =>
    // FIXME: this feels like unnecessary lifting when neither of these steps actually require a player
    // Or maybe I'd have to rewrite these functions as well but that's weird because this is the player
    val saltAndAdd = ReaderT.liftF(v.useSalt(card)).flatMap(v.addToCauldron)
    val ifSalt     = (saltAndAdd(this) *> v.sell).runS(board).value
    val ifNoSalt   = v.sell.runS(board).value
    val diff       = ifSalt.gold - ifNoSalt.gold
    board.saltAvailable && diff > 0
  }
}
case object Goblin extends Player {
  def willFlip(v: VersionTrait): BoardState[Boolean]                      = State.pure(true)
  override def willSalt(v: VersionTrait, card: Card): BoardState[Boolean] = State.pure(true)
}
type BoardState[A]     = State[PlayerBoard, A]
type WithPlayer[A]     = Reader[Player, A]
type PlayerAndState[A] = ReaderT[BoardState, Player, A]
def id2BoardState = new (Id ~> BoardState) {
  def apply[T](f: Id[T]): BoardState[T] = State.pure(f)
}

trait VersionTrait {
  val flipEffects: WithPlayer[Compendium]
  val sellEffects: Compendium
  val supply: Cards
  val flip: PlayerAndState[Card] // doesn't use player
  def resolveFlipEffects(card: Card): PlayerAndState[Card]
  def useSalt(card: Card): BoardState[Card]
  def decideToSalt(card: Card): PlayerAndState[Card] =
    ReaderT {
      _.willSalt(this, card).flatMap(willSalt => if (willSalt) useSalt(card) else State.pure(card))
    }
  def addToCauldron(card: Card): PlayerAndState[Unit] // doesn't use player
  def scorePotion: BoardState[Unit]
  def resolveSellEffects: BoardState[Unit] // doesn't use player (but could)
  val flipOnce: PlayerAndState[Unit]
  def sell: BoardState[Unit]
}

object V2_1 extends VersionTrait {
  import Ingredient.*
  val supply: Cards = for {
    ingr  <- Ingredient.values.toVector
    grade <- Vector(1, 2, 3)
    card  <- Card(grade, ingr) * 4
  } yield card

  val flipEffects: WithPlayer[Compendium] = Reader { player =>
    {
      case Card(2, Fungus, _) =>
        State.modify { b =>
          b.copy(limit = b.limit + 1)
        }
      case target @ Card(2, Slime, _) =>
        State.get.flatMap { b =>
          val possibles =
            b.cauldron.filter(c => flipEffects(player).contains(c) && c.subtype != Slime)
          // hypothetically resolve each effect. resolveFlipEffects currently assumes the resolver is in the A channel of State
          val currentGold = sell.runS(b).value.gold
          val bestOption = possibles.maxByOption(c =>
            // look up the current card from the cauldron and calculate the profit if we resolved its effect again
            val flipAndAdd = flipEffects.mapK(id2BoardState).map(_.get(c)) *> addToCauldron(target)
            val profit =
              (flipAndAdd(player) *> sell)
                .runS(b)
                .value
                .gold - currentGold
            println(s"resolving as $c yields $profit")
            profit
          )
          bestOption.fold(State.pure(()))(flipEffects(player).apply)
        }
      case Card(2, Mineral, _) =>
        State.modify { b =>
          // +1G
          b.copy(gold = b.gold + 1)
        }
      case target @ Card(2, Viscera, _) =>
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
            // annoying typing issue here
            val addAndFlip = addToCauldron(target) *> flipOnce
            val profit = (addAndFlip(player) *> sell)
              .runS(b.copy(deck = d))
              .value
              .gold - currentGold
            //         println(s"putting ${d.headOption} on top yields $profit")
            profit
          }
          // println(s"${highestScoringDeck} yields highest G")
          // the highest EV will be on top, the rest bottomed
          b.copy(deck = highestScoringDeck.getOrElse(b.deck))
        }
      case Card(2, Flora, _) =>
        State.modify { b =>
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
      case Card(2, Soil, _) =>
        State.modify { b =>
          // cure a 3
          b.cauldron.zipWithIndex
            .find((c, _) => c.grade == 3 && !c.cured)
            .fold(
              b
            ) { case (card, idx) =>
              b.copy(
                cauldron = b.cauldron.updated(idx, card.copy(cured = true))
              )
            }
        }
    }
  }
  // partial for now
  val sellEffects: Compendium = { case Card(3, Slime, _) =>
    State.modify { b =>
      b.modifyGold(_ + b.cauldron.count(_.grade == 2))
    }
  }

  val flip: PlayerAndState[Card] = ReaderT.liftF(
    State { b =>
      // TODO: What to do (besides match error) if deck is empty?
      // This creates a new bug with Viscera-2 if it's the last card, because I try to flip from an empty deck
      // the previous implementation that used flipped: Option[Card] could just no-op if the deck is empty
      b.deck match {
        case (top: Card) +: (rest: Cards) =>
          b.copy(deck = rest) -> top
      }
    }
  )

  // Should Compendium return a Midflip? I guess it only makes sense for flipEffects, not sellEffects.
  // Maybe Compendium as a trait is generic in output
  // Compendium[A] { def get(card: Card): State[PlayerBoard, A] }
  // sellEffects: Reader[Player, Compendium[Unit]]
  // flipEffects: Reader[Player, Compendium[Card]]
  def resolveFlipEffects(card: Card): PlayerAndState[Card] =
    for {
      comp <- flipEffects.mapK(id2BoardState)
      x <- ReaderT.liftF(
        comp.get(card).fold(State.pure(card))(_.map(_ => card))
      )
    } yield x

  override def useSalt(card: Card): BoardState[Card] =
    State[PlayerBoard, Card] { b =>
      if (b.saltAvailable) {
        b.modifyGold(_ - 3).copy(saltAvailable = false) -> card.copy(cured = true)
      } else {
        b -> card
      }
    }

  def addToCauldron(card: Card): PlayerAndState[Unit] = ReaderT.liftF(
    State.modify(b => b.copy(cauldron = b.cauldron :+ card))
  )

  override def scorePotion: BoardState[Unit] = State.modify { board =>
    board.modifyGold(_ + board.cauldron.size)
  }

  override def resolveSellEffects: BoardState[Unit] = for {
    board <- State.get
    _ <- board.cauldron
      .map(c => sellEffects.getOrElse(c, State.pure(())))
      .fold(State.pure(()))(_ *> _)
  } yield ()

  // ReaderT enables this composition, but makes other compositions more annoying
  val flipOnce: PlayerAndState[Unit] =
    flip >>= resolveFlipEffects >>= decideToSalt >>= addToCauldron

  val sell: BoardState[Unit] = State.get.flatMap {
    case b if b.exploded => scorePotion
    case _               => scorePotion *> resolveSellEffects
  }
}

case class NewSim(
    version: VersionTrait
) {

  import version.*

  def brew(player: Player, verbose: Boolean = false): Step = {
    def flipOrPass: Step = for {
      b <- State.get[PlayerBoard]
      _ = if (verbose) println(s"$player board: ${b.format}")
      wantsToFlip <- player.willFlip(version)
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
        case _ if wantsToFlip =>
          if (verbose) println(s"$player didn't explode, wants to go again")
          flipOnce(player) *> flipOrPass
        // didn't explode, have more cards, don't want to flip
        case _ =>
          if (verbose) println(s"$player didn't explode, wants to pass")
          sell
      }
    } yield ()

    flipOrPass
  }
}
