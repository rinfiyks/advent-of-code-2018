package puzzles

object Day7 extends Puzzle with App {

  override val day = 7

  case class Instruction(step1: String, step2: String)

  val instructions = input.map(line => Instruction(line.split(" ")(1), line.split(" ")(7)))
  val steps = instructions.flatMap(i => List(i.step1, i.step2)).sorted.distinct

  private def findUnstartableSteps(completedSteps: List[String]): List[String] =
    instructions.filterNot(i => completedSteps contains i.step1).map(_.step2).distinct

  def part1: String = {
    @annotation.tailrec
    def recurse(completedSteps: List[String], remainingSteps: List[String]): String = {
      val unstartableSteps = instructions.filterNot(i => completedSteps contains i.step1).map(_.step2).distinct
      remainingSteps.find(s => !unstartableSteps.contains(s)) match {
        case Some(step) => recurse(step :: completedSteps, remainingSteps.filterNot(_ == step))
        case None => completedSteps.reverse.mkString
      }
    }

    recurse(Nil, steps)
  }

  def part2: Int = {
    case class Worker(step: String, endTime: Int)
    val workerCount = 5

    @annotation.tailrec
    def recurse(completedSteps: List[String], remainingSteps: List[String], workers: List[Worker], currentTime: Int): Int = {
      val completedWorkers = workers.filter(_.endTime == currentTime)
      val nextCompletedSteps = completedWorkers.map(_.step) ::: completedSteps
      val unstartableSteps = findUnstartableSteps(nextCompletedSteps)
      val availableSteps = remainingSteps diff unstartableSteps
      val oldWorkers = workers diff completedWorkers
      val newSteps = availableSteps.take(workerCount - oldWorkers.length)
      val newWorkers = newSteps.map(step => Worker(step, currentTime + step.head.toInt - 4))
      val nextWorkers = newWorkers ::: oldWorkers
      if (nextWorkers.isEmpty) currentTime
      else {
        val nextTime = nextWorkers.map(_.endTime).min
        recurse(nextCompletedSteps, remainingSteps diff newSteps, nextWorkers, nextTime)
      }
    }

    recurse(Nil, steps, Nil, 0)
  }

  println(part1)
  println(part2)
}
