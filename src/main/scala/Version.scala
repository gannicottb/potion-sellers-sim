package games.wrg

import simulators.simulator.{Compendium, PlayerBoard, Step}

import cats.syntax.all.*

case class Version(
    flipEffects: Compendium,
    sellEffects: Compendium,
    flipOnce: Step,
    sell: Step,
    initBoard: Cards => PlayerBoard
)
