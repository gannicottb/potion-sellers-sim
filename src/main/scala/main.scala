package games.wrg

import cats.effect.{IO, IOApp}
import cats.effect.std.Random
import cats.syntax.all.*
import simulator.{BoardState, Gambler, StarterDeck, TestCase, VerySafe}

object Main extends IOApp.Simple {
  def report(boardState: BoardState) = for {
    _ <- IO.println(boardState.cauldron)
    _ <- {
      if (boardState.exploded) IO.println("BOOM!")
      else
        IO.println(
          s"${boardState.cauldron.length} cards flipped, total grade = ${boardState.cauldron.map(_.grade).sum}"
        )
    }
  } yield ()

  def summary(batch: List[BoardState]) = for {
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
  } yield ()

  val run = {
    val seed = Random.scalaUtilRandom[IO]
    for {
      _ <- List(VerySafe, Gambler).flatMap(strat =>
        simulator.StarterDeck.values.map(deck => TestCase(strat, deck)).toList
      ).traverse(tc =>
        IO.println(tc) *> simulator.run[IO](seed, 100, tc).flatMap(summary)
      )
    } yield ()
  }
}
