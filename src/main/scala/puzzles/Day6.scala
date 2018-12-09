package puzzles

object Day6 extends Puzzle with App {

  override val day = 6

  case class TargetCoordinate(id: Int, x: Int, y: Int) {
    def manhattan(x: Int, y: Int): Int = math.abs(this.x - x) + math.abs(this.y - y)
  }

  val targets: List[TargetCoordinate] = input.zipWithIndex.map { case (line, id) =>
    val s = line.split(", ").map(_.toInt)
    TargetCoordinate(id, s(0), s(1))
  }

  val minWidth = targets.map(_.x).min
  val maxWidth = targets.map(_.x).max
  val minHeight = targets.map(_.y).min
  val maxHeight = targets.map(_.y).max

  def part1: Int = {
    case class Point(x: Int, y: Int, closestTarget: Option[TargetCoordinate])

    def targetFromDistances(distances: List[(TargetCoordinate, Int)]): Option[TargetCoordinate] =
      distances.sortBy(_._2) match {
        case d1 :: d2 :: _ if d1._2 != d2._2 => Some(d1._1)
        case _ => None
      }

    val points = for {
      x <- minWidth to maxWidth
      y <- minHeight to maxHeight
      distances = targets.map(t => (t, t.manhattan(x, y)))
    } yield Point(x, y, targetFromDistances(distances))

    val edgeTargets = points.filter(p => p.x == minWidth || p.x == maxWidth || p.y == minHeight || p.y == maxHeight)
      .flatMap(_.closestTarget).distinct

    val nonInfinitePoints = points.filterNot(p => edgeTargets.exists(e => p.closestTarget contains e))
    val largestArea = nonInfinitePoints.groupBy(_.closestTarget).mapValues(_.length).maxBy(_._2)._2
    largestArea
  }

  def part2: Int = {
    case class Point(x: Int, y: Int, totalDistance: Int)

    val points = for {
      x <- minWidth to maxWidth
      y <- minHeight to maxHeight
      totalDistance = targets.map(_.manhattan(x, y)).sum
    } yield Point(x, y, totalDistance)

    points.count(_.totalDistance < 10000)
  }

  println(part1)
  println(part2)
}

