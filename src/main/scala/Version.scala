package games.wrg

import stateful.{PlayerBoard, Step}

import cats.syntax.all.*

case class Version(
    flipEffects: Map[Card, Step],
    sellEffects: Map[Card, Step],
    flipOnce: Step,
    sell: Step,
    initBoard: Cards => PlayerBoard
)
