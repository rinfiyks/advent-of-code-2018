package puzzles

trait Puzzle {

  val day: Int

  lazy val input: List[String] = io.Source.fromResource(s"puzzles/day$day.txt").getLines.toList

}
