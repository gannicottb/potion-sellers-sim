package games.wrg

import stateful.{Compendium, PlayerBoard, Step}

import cats.syntax.all.*

case class Version(
    flipEffects: Map[Card, Step]|Compendium,
    sellEffects: Map[Card, Step]|Compendium,
    flipOnce: Step,
    sell: Step,
    initBoard: Cards => PlayerBoard
)
