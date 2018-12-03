package puzzles

object Day1 extends Puzzle with App {

  override val day = 1

  def part1: Int = input.map(_.toInt).sum

  def part2: Int = recurse(input, 0, Set(0))

  @annotation.tailrec
  private def recurse(remainingInput: List[String], lastFrequency: Int, frequencies: Set[Int]): Int = {
    val nextFrequency = lastFrequency + remainingInput.head.toInt
    if (frequencies contains nextFrequency) nextFrequency
    else {
      val nextInput = remainingInput.tail match {
        case Nil => input
        case _ => remainingInput.tail
      }
      recurse(nextInput, nextFrequency, frequencies + nextFrequency)
    }
  }

  println(part1)
  println(part2)
}
