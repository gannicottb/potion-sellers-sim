package games.wrg

import simulators.{Sim_2_1, Sim_2_2}
import games.wrg.simulators.simulator.*

import cats.effect.std.Random
import cats.effect.{IO, IOApp}
import cats.syntax.all.*

object Main extends IOApp.Simple {
  type SimResult = (SimCase, List[PlayerBoard])
  extension [A](l: List[A]) def average(fn: List[A] => Int): Double = fn(l) / l.size.toDouble
  def formatTableSim(results: List[SimResult]): String = {
    val preamble = s"${results.head._2.length} repetitions each. All values in average G."
    val header   = " " * 18 + results.map(_._1.startingDeck.label).distinct.mkString(" " * 4)
    val table = results
      .groupBy { case (tc, res) => tc.player }
      .toList
      .map { case (strat, tcAndResults) =>
        val allFinalStates = tcAndResults.flatMap(_._2)
        val min            = allFinalStates.minBy(_.gold)
        val max            = allFinalStates.maxBy(_.gold)
        val avgPotionSize  = allFinalStates.average(_.map(_.cauldron.size).sum)
        val pctExploded    = allFinalStates.average(_.count(_.exploded))
        val totalAvg       = allFinalStates.average(_.map(_.gold).sum)
        val tableRow = List(
          s"$strat${" " * (16 - strat.toString.length)}|",
          tcAndResults
            .map { case (tc, res) => f"${res.map(_.gold).sum / res.size.toDouble}%1.2f" }
            .mkString(" "),
          f"| $totalAvg%1.2f (min: ${min.gold}G, max: ${max.gold}G, avg potion:$avgPotionSize%1.2f exploded?: ${pctExploded * 100}%1.2f%%)"
        ).mkString(" ")
        totalAvg -> tableRow
      }
      .sortBy(_._1)
      .reverse // best performers up top
      .collect { case (_, line) => line } // discard sort key
      .mkString("\n")

    List(
      preamble,
      header,
      table
    ).mkString("\n")
  }

  val run: IO[Unit] = {
    val seed    = Random.scalaUtilRandom[IO]
    val players = List(Gambler(0.0), Gambler(.5), Gambler(1.0), VibesBased(0.5), VibesBased(0.8), EVCalc())

    val starterCases = players
      .flatMap(player =>
        StarterDeck.values.toList
          .map(d => SimCase(player, LabeledDeck(d)))
      )
    val customCases = players
      .flatMap(player =>
        CustomDeck.values.toList
          .map(d => SimCase(player, LabeledDeck(d)))
      )

    def runAndReport(
        label: String
    )(cases: List[SimCase], simulateWith: SimCase => IO[List[PlayerBoard]]) = for {
      res <- cases.map(sc => simulateWith(sc).map(sc -> _)).parSequence
      _   <- IO.println(label)
      _   <- IO.println(s"${res.size} results")
      _   <- IO.println(formatTableSim(res))
    } yield ()

    for {
      // TODO: take the supply and build N M-card decks from it. Have Colinbot run the flip for each and print
      //    the best performers
      // TODO: Remember that while interpreting this is only about playing out a deck, not finding the best
      //     overall strategy. Multiple strategies are interesting but only insofar as it informs us about
      //     the earning potential of certain cards and combinations.
      // TODO: Implementing Salt use could be interesting
      _ <- runAndReport("v2.1 - Starter")(starterCases, Sim_2_1.simulator.runShuffled(seed, 720, _))
      _ <- runAndReport("v2.1 - Custom")(customCases, Sim_2_1.simulator.runShuffled(seed, 720, _))
      _ <- runAndReport("v2.2 - Starter")(starterCases, Sim_2_2.simulator.runShuffled(seed, 720, _))
      _ <- runAndReport("v2.2 - Custom")(customCases, Sim_2_2.simulator.runShuffled(seed, 720, _))
//      _ <- runAndReport("v2.2 - Colin")(
//        List(SimCase(EVCalc(true), LabeledDeck(CustomDeck.S))),
//        Sim_2_2.simulator.runShuffled(seed, 1, _)
//      )
//      c <- Sim_2_2.simulator.runShuffled(seed, 1, SimCase(EVCalc(true), LabeledDeck(StarterDeck.A)))
//      _ <- IO.println("Colin EV")
//      _ <- IO.println(s"$c")
    } yield ()
  }
}
