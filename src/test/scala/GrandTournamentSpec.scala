package games.wrg

import cats.effect.IO
import cats.effect.std.Random
import cats.syntax.show.*
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import cats.effect.unsafe.implicits.global
import Cards.given
import games.wrg.simulators.Sim_2_2

class GrandTournamentSpec extends AnyWordSpec with Matchers {
  "GrandTournament" should {
    "split supply in random chunks" in {
      val io = for {
        given Random[IO] <- Random.scalaUtilRandom[IO]
        shuffled <- Random[IO].shuffleVector(Sim_2_2.supply)
        results <- GrandTournament.splitSupply(shuffled, 4 to 10)
      } yield {
        println(results.map(_.show).mkString("\n"))
        results.map(_.size).max <= 10 shouldBe true
      }

      io.unsafeRunSync()
    }
  }

}
