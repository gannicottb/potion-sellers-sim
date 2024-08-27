package games.wrg

import stateful.Step
import cats.syntax.all.*

case class Version(
    flipEffects: Map[Card, Step],
    sellEffects: Map[Card, Step],
    flipOnce: Step,
    sell: Step
)
