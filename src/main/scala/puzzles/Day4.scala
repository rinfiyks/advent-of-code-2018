package puzzles

object Day4 extends Puzzle with App {

  override val day = 4

  case class Guard(id: Int, sleepLog: List[Int])

  private def parseInput: List[Guard] = {
    type GuardMap = collection.mutable.HashMap[Int, List[Int]]

    @annotation.tailrec
    def recurse(remainingInput: List[String], guardId: Int, asleepMinute: Int, guardMap: GuardMap): GuardMap = {
      remainingInput match {
        case Nil => guardMap
        case h :: t =>
          if (h contains "begins shift") recurse(t, h.split("[ #]")(4).toInt, 0, guardMap)
          else if (h contains "wakes up") {
            val wakeupMinute = h.split("[:\\]]")(1).toInt
            val guardSleepLog = guardMap.getOrElseUpdate(guardId, List.empty[Int])
            guardMap.update(guardId, (asleepMinute until wakeupMinute).toList ::: guardSleepLog)
            recurse(t, guardId, 0, guardMap)
          } else {
            recurse(t, guardId, h.split("[:\\]]")(1).toInt, guardMap)
          }
      }
    }

    recurse(input.sorted, 0, 0, collection.mutable.HashMap.empty[Int, List[Int]])
      .toList.map(l => Guard(l._1, l._2))
  }

  def part1: Int = {
    val sleepiestGuard = parseInput.maxBy(_.sleepLog.length)
    val sleepiestMinute = sleepiestGuard.sleepLog.groupBy(identity).mapValues(_.length).maxBy(_._2)._1
    sleepiestGuard.id * sleepiestMinute
  }

  def part2: Int = {
    val a = parseInput
    val sleepiestGuardBySameMinute = a.maxBy(_.sleepLog.groupBy(identity).mapValues(_.length).maxBy(_._2)._2)
    val minute = sleepiestGuardBySameMinute.sleepLog.groupBy(identity).mapValues(_.length).maxBy(_._2)._1
    sleepiestGuardBySameMinute.id * minute
  }

  println(part1)
  println(part2)
}
