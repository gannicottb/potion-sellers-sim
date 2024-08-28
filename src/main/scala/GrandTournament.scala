package games.wrg

import simulators.simulator.{LabeledDeck, Player, PlayerBoard, SimCase}
import Cards.given
import cats.Monad
import cats.effect.std.Random
import cats.syntax.all.*
import games.wrg.Main.SimResult

object GrandTournament {

  /** Premise: Take a full Supply generate 4 decks of X cards from it randomly have each deck brew N
    * times, collect stats (repeat above and collect stats)
    */

  def run[F[_]: Random: Monad](supply: Cards, deckSize: Int, player: Player)(
      sim: SimCase => F[List[PlayerBoard]]
  ): F[List[SimResult]] = for {
    shuffled <- Random[F].shuffleVector(supply)
    decks = shuffled.grouped(deckSize).take(4).toList
    _     = println(s"${decks.mapWithIndex { case (d, i) => (i, d.show) }.mkString("\n")}")
    results <- decks
      .mapWithIndex { case (deck, i) => SimCase(player, LabeledDeck(s"$i", deck)) }
      .traverse(sc => sim(sc).map(sc -> _))
  } yield results
}
