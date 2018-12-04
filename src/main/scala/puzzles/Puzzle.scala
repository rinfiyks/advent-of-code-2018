package puzzles

trait Puzzle {

  val day: Int

  lazy val input: List[String] = io.Source.fromResource(s"puzzles/day$day.txt").getLines.toList

  def time[A](f: => A): A = {
    val start = System.currentTimeMillis()
    val result = f
    val end = System.currentTimeMillis()
    println(s"${end - start} ms")
    result
  }

}
