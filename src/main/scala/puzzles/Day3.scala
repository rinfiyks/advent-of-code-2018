package puzzles

object Day3 extends Puzzle with App {

  override val day = 3

  case class Claim(id: Int, x: Int, y: Int, width: Int, height: Int)

  val claims: List[Claim] = input.map { line =>
    val split = line.split("[^\\d]").filter(_.nonEmpty).map(_.toInt)
    Claim(split(0), split(1), split(2), split(3), split(4))
  }

  val fabricWidth = claims.map(c => c.x + c.width).max
  val fabricHeight = claims.map(c => c.y + c.height).max

  val fabric = fillFabric

  private def fillFabric: Vector[Vector[Int]] = {
    val fabric = Vector.fill(fabricWidth)(collection.mutable.ArrayBuffer.fill(fabricHeight)(0))
    claims.foreach { claim =>
      for {
        x <- claim.x until claim.x + claim.width
        y <- claim.y until claim.y + claim.height
        old = fabric(x)(y)
      } yield fabric(x).update(y, old + 1)
    }
    fabric.map(_.toVector)
  }

  def part1: Int = fabric.flatten.count(_ > 1)

  def part2: Option[Claim] =
    claims.find { claim =>
      (for {
        x <- claim.x until claim.x + claim.width
        y <- claim.y until claim.y + claim.height
      } yield fabric(x)(y))
        .forall(_ == 1)
    }

  println(part1)
  println(part2)
}
