package games.wrg

import simulators.{Sim_2_1, Sim_2_2}
import stateful.*

import cats.effect.std.Random
import cats.effect.{IO, IOApp}
import cats.syntax.all.*

object Main extends IOApp.Simple {
  type SimResult = (SimCase, List[PlayerBoard])

  def formatTableSim(results: List[SimResult]): String = {
    val preamble = s"${results.head._2.length} repetitions each. All values in average G."
    val header   = " " * 18 + results.map(_._1.startingDeck.label).distinct.mkString(" " * 4)
    val table = results
      .groupBy { case (tc, res) => tc.player }
      .toList
      .map { case (strat, tcAndResults) =>
        val allFinalStates = tcAndResults.flatMap(_._2)
        val total          = allFinalStates.map(_.gold).sum / allFinalStates.length.toDouble
        val tableRow = List(
          s"$strat${" " * (16 - strat.toString.length)}|",
          tcAndResults
            .map { case (tc, res) => f"${res.map(_.gold).sum / res.length.toDouble}%1.2f" }
            .mkString(" "),
          f"| $total%1.2f"
        ).mkString(" ")
        total -> tableRow
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
    val seed = Random.scalaUtilRandom[IO]
    val simCases = List(Gambo(0.0), Gambo(.1), Gambo(.5), Gambo(.75), Gambo(1.0), EVCalc())
      .flatMap(player =>
        (TestDeck.values.toList)
          .map(d => SimCase(player, LabeledDeck(d)))

      )

    for {
      // TODO: take the supply and build N M-card decks from it. Have Colinbot run the flip for each and print
      //    the best performers
      res1 <- simCases.map(sc =>
//        IO(sc -> Sim_2_1.simulator.runPermutations(sc))
        Sim_2_1.simulator.runShuffled(seed, 720, sc).map(sc -> _)
      ).parSequence
      _ <- IO.println("v2.1")
      _ <- IO.println(s"${res1.size} results")
      _ <- IO.println(formatTableSim(res1))

      res <- simCases.map(sc =>
//        IO(sc -> Sim_2_2.simulator.runPermutations(sc))
        Sim_2_2.simulator.runShuffled(seed, 720, sc).map(sc -> _)
      ).parSequence // run each case in parallel
      _ <- IO.println("v2.2")
      _ <- IO.println(s"${res1.size} results")
      _ <- IO.println(formatTableSim(
        res
      ))

      c <- Sim_2_2.simulator.runShuffled(seed, 1, SimCase(EVCalc(true), LabeledDeck(StarterDeck.A)))
      _ <- IO.println("Colin EV")
      _ <- IO.println(s"$c")
    } yield ()
  }
}
