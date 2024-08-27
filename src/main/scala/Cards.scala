package games.wrg

type Cards = Vector[Card]
object Cards {
  extension (cs: Cards) {
    def putCardOnTop(index: Int): Cards = if (index < cs.size) {
      val (top, bottom)               = cs.splitAt(index + 1)
      val (restOfTop, Vector(target)) = top.splitAt(top.length - 1)
      target +: (restOfTop ++ bottom)
    } else cs
  }
}
