package games.wrg

import cats.Show
import cats.syntax.show.*

type Cards = Vector[Card]
object Cards {
  extension (cs: Cards) {
    def putCardOnTop(index: Int): Cards = if (index < cs.size) {
      val (top, bottom)               = cs.splitAt(index + 1)
      val (restOfTop, Vector(target)) = top.splitAt(top.length - 1)
      target +: (restOfTop ++ bottom)
    } else cs
  }

  given Show[Cards] = Show.show(cs =>
    s"(${cs.size})[" + cs.map(_.show).mkString(", ") + "]"
  )

  def empty = Vector.empty[Card]

  def apply(c: Card | Cards*): Cards = {
    c.toVector.foldLeft(Cards.empty) {
      case (res, cardOrCards) => cardOrCards match {
        case c: Card => res :+ c
        case cs: Cards => res ++ cs
      }
    }
  }
}
