package puzzles

object Day2 extends Puzzle with App {

  override val day = 2

  def part1: Int = {
    val counts = input.flatMap(duplicateLetterCounts)
    counts.count(_ == 2) * counts.count(_ == 3)
  }

  private def duplicateLetterCounts(s: String): List[Int] =
    s.groupBy(identity).values.map(_.length).filterNot(_ == 1).toList.distinct

  def part2: String = recurse(input)

  private def recurse(ids: List[String]): String = {
    val (currentId, idsToCheck) = (ids.head, ids.tail)
    val result = idsToCheck.map(countDifferences(currentId)).filter(_.length == 1)
    result match {
      case Nil => recurse(idsToCheck)
      case r :: _ => currentId.slice(0, r.head) + currentId.slice(r.head + 1, 26)
    }
  }

  private def countDifferences(id1: String)(id2: String): List[Int] =
    (0 until id1.length).filterNot(i => id1(i) == id2(i)).toList

  println(part1)
  println(part2)
}
