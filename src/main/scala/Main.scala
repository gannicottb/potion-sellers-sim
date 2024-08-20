package games.wrg

import cats.effect.{IO, IOApp}
import cats.effect.std.Random
import cats.syntax.all.*
import simulator.*

object Main extends IOApp.Simple {
  def report(boardState: BoardState): IO[Unit] = for {
    _ <- IO.println(boardState.cauldron)
    _ <- {
      if (boardState.exploded) IO.println("BOOM!")
      else
        IO.println(
          s"${boardState.cauldron.length} cards flipped, total grade = ${boardState.cauldron.map(_.grade).sum}"
        )
    }
  } yield ()

  private def summary(batch: List[BoardState]) = for {
//    _ <- batch.traverse(report)
    _ <- IO.println(
      s"Avg cards flipped = ${batch.map(_.cauldron.size).sum / batch.size.toDouble}"
    )
    _ <- IO.println(
      s"Avg cards cured = ${batch.map(_.cauldron.count(_.cured)).sum / batch.size.toDouble}"
    )
    _ <- IO.println(
      s"Avg Gold earned = ${batch.map(_.gold).sum / batch.size.toDouble}"
    )
    _ <- IO.println(
      s"Explosion rate = ${batch.count(_.exploded) / batch.size.toDouble}"
    )
  } yield ()
  type TestResult = (TestCase, List[BoardState])
  def formatTable(results: List[TestResult]): String = {
    val preamble = s"${results.head._2.length} repetitions each"
    val header = " " * 18 + results.map(_._1.starterDeck.toString).distinct.mkString(" " * 4)
    val table = results.groupBy { case (tc, res) => tc.strategy }.map {
      case (strat, tcAndResults) =>
        List(
          s"$strat${" " * (16 - strat.toString.length)}|",
          tcAndResults.map { case (tc, res) => f"${res.map(_.gold).sum / res.length.toDouble}%1.2f" }.mkString(" ")
        ).mkString(" ")
    }.mkString("\n")

    List(
      preamble, header, table
    ).mkString("\n")
  }


  val run: IO[Unit] = {
    val seed = Random.scalaUtilRandom[IO]
    val testCases = List(VerySafe, Gambler, Colin()).flatMap(strat =>
      simulator.StarterDeck.values.map(deck => TestCase(strat, deck)).toList
    )
    for {
//      _ <- List(VerySafe, Gambler, Colin).flatMap(strat =>
//        simulator.StarterDeck.values.map(deck => TestCase(strat, deck)).toList
//      ).traverse(tc =>
//        IO.println(tc) *> simulator.run[IO](seed, 1, tc).flatMap(summary)
//      )
      results <- testCases.traverse(tc =>
        simulator.run[IO](seed, 100, tc).map(tc -> _)
      )
      _ <- IO.println(formatTable(results))
    } yield ()
  }
}
