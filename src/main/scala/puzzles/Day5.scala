package puzzles

object Day5 extends Puzzle with App {

  override val day = 5

  @annotation.tailrec
  private def recurse(before: List[Char], after: List[Char]): Int = {
    (before, after) match {
      case (_, Nil) => before.reverse.mkString.trim.length
      case (bh :: bt, ah :: at) =>
        if (ah.toUpper == bh.toUpper && ah != bh) {
          recurse(bt, at)
        } else recurse(ah +: before, at)
      case _ =>
        0
    }
  }

  def part1: Int = {
    val inputAsCharList = input.head.toList
    recurse(List(' '), inputAsCharList)
  }

  def part2: Int = {
    val inputAsCharList = input.head.toList
    ('a' to 'z').map { char =>
      recurse(List(' '), inputAsCharList.filterNot(_.toLower == char))
    }.min
  }

  println(part1)
  println(part2)
}
