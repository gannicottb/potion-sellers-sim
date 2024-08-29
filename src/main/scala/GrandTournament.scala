package games.wrg

import simulators.simulator.{LabeledDeck, Player, PlayerBoard, SimCase}
import Cards.given
import Card.given

import cats.{Monad, Parallel, Show}
import cats.effect.std.Random
import cats.syntax.all.*
import games.wrg.Main.SimResult
import Main.average

import cats.effect.kernel.Resource.Pure

object GrandTournament {

  /** Premise: Take a full Supply
    *
    * generate 4 decks of X cards from it (to simulate a 4-player game)
    *
    * randomly have each deck brew N times, collect stats (repeat above and collect stats)
    */

  case class Stats(average: Double, min: Int, max: Int)
  object Stats {
    def apply(results: List[PlayerBoard], accessor: PlayerBoard => Int): Stats =
      Stats(
        results.average(_.map(accessor).sum),
        accessor(results.minBy(accessor)),
        accessor(results.maxBy(accessor))
      )
    given Show[Stats] = Show.show(st =>
      f"${st.average}%1.2f (${st.min}, ${st.max})"
    )
  }

  def formatStatMap[K: Show](label: String, m: Map[K, Stats], limit: Int) = {
    val header = s"** $label **"
    val rows = m.toList
      .sortBy { case (_, stats) => -stats.average }
      .take(limit)
      .map { case (k, stats) => s"${k.show}: ${stats.show}" }
      .mkString("\n")

    List(header, rows).mkString("\n")
  }

  def makeDeckPerformanceMap(results: List[SimResult]): Map[Cards, Stats] = {
    results.map { case (sc, results) =>
      sc.startingDeck.cards -> Stats(results, _.gold)
    }.foldLeft(Map.empty[Cards, Stats]) { case (m, (cards, stats)) =>
      m.updatedWith(cards) {
        case Some(old) => Some(stats.copy(average = (old.average + stats.average) / 2))
        case None      => Some(stats)
      }
    }
  }

  def makeCardPerformanceMap(results: List[SimResult]): Map[Card, Stats] = {
    results
      .flatMap { case (sc, results) =>
        sc.startingDeck.cards.distinct.map(_ ->
          Stats(results, _.gold)
        )
      }.foldLeft(Map.empty[Card, Stats]) { case (m, (card, stats)) =>
        m.updatedWith(card) {
          case Some(old) => Some(stats.copy(average = (old.average + stats.average) / 2))
          case None      => Some(stats)
        }
      }
  }

  def splitSupply[F[_]: Random: Monad](supply: Cards, sizeRange: Range): F[List[Cards]] = {
    // split up the supply and return a list of decks with sizes that fall in sizeRange
    def loop(remaining: Cards, result: List[Cards]): F[List[Cards]] =
      if (remaining.isEmpty) Monad[F].pure(result)
      else
        Random[F]
          .betweenInt(sizeRange.min, sizeRange.max + 1)
          .flatMap(size => loop(remaining.drop(size), result :+ remaining.take(size)))

    loop(supply, List.empty[Cards])
  }

  def runNextGroupOfDecks[F[_]: Random: Monad](
      index: Int,
      supply: Cards,
      deckSize: Int, // a range? what about leftover cards?
      player: Player
  )(
      simulateWith: SimCase => F[List[PlayerBoard]]
  ): F[List[SimResult]] = for {
    shuffled <- Random[F].shuffleVector(supply)
    decks    <- splitSupply(shuffled, 4 to 10)
    results <- decks
      .mapWithIndex { case (deck, i) =>
        val deckId = s"$index.$i"
        // println(s"${decks.mapWithIndex { case (d, i) => (deckId, d.show) }.mkString("\n")}")
        SimCase(player, LabeledDeck(s"$deckId", deck))
      }
      .traverse(sc => simulateWith(sc).map(sc -> _))
  } yield results

  def run[F[_]: Random: Monad: Parallel](supply: Cards, deckSize: Int, player: Player)(
      simulateWith: SimCase => F[List[PlayerBoard]]
  ): F[List[SimResult]] = for {
    results <- (0 to 100).toList.map(idx =>
      runNextGroupOfDecks[F](idx, supply, deckSize, player)(simulateWith)
    ).parFlatSequence
  } yield {
    println(formatStatMap("Card Performance", makeCardPerformanceMap(results), 15))
    println(formatStatMap("Deck Performance", makeDeckPerformanceMap(results), 15))
    results
  }
}
