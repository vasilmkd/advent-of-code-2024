import scala.io.Source

object Day2:
  case class Report(levels: Array[Int]):
    def isSafe: Boolean =
      inline def diff(x: Int, y: Int): Boolean =
        var d = x - y
        if d < 0 then d = -d
        d >= 1 && d <= 3
      inline def pairwise(inline p: (Int, Int) => Boolean): Boolean =
        var index = 0
        var cont = true
        while cont && index < levels.length - 1 do
          val x = levels(index)
          val y = levels(index + 1)
          cont = p(x, y) && diff(x, y)
          index += 1
        cont
      inline def increasing: Boolean = pairwise(_ < _)
      inline def decreasing: Boolean = pairwise(_ > _)
      increasing || decreasing

    def isSafeByRemovingALevel: Boolean =
      def withSkipped(index: Int): Report =
        val xs = levels.take(index)
        val ys = levels.drop(index + 1)
        Report(xs ++ ys)
      levels.indices.exists(withSkipped(_).isSafe)

  object Report:
    def parse(line: String): Report =
      Report(line.split(" ").map(_.toInt))

  def readInput(): LazyList[Report] =
    Source
      .fromResource("day2.txt")
      .getLines()
      .map(Report.parse)
      .to(LazyList)

  def part1(): Int =
    val reports = readInput()
    reports.count(_.isSafe)

  def answer1: Int = 321

  def part2(): Int =
    val reports = readInput()
    reports.count(_.isSafeByRemovingALevel)

  def answer2: Int = 386
