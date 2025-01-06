type Day = {
  def part1(): Any
  def answer1: Any
  def part2(): Any
  def answer2: Any
}

private def runDay(day: Day): Unit =
  import scala.reflect.Selectable.reflectiveSelectable
  val dayName = day.getClass.getSimpleName.stripSuffix("$")
  print(s"$dayName")
//  val part1 = day.part1()
  val part1 = timed(day.part1())
  assert(part1 == day.answer1, s"part 2: expected: ${day.answer1}, actual: $part1")
  print(s"part 1: $part1")
  print(s", ")
//  val part2 = day.part2()
  val part2 = timed(day.part2())
  assert(part2 == day.answer2, s"part 2: expected: ${day.answer2}, actual: $part2")
  println(s"part 2: $part2")
  println()

inline def timed[A](inline block: => A): A =
  import scala.concurrent.duration.DurationLong
  val start = System.nanoTime()
  val res = block
  val end = System.nanoTime()
  println()
  val millis = (end - start).nanoseconds.toMillis
  println(s"Elapsed time: $millis ms")
  res

@main
def main(): Unit =
  runDay(Day1)
  runDay(Day2)
  runDay(Day3)
  runDay(Day4)
  runDay(Day5)
  runDay(Day6)
  runDay(Day7)
  runDay(Day8)
  runDay(Day9)
  runDay(Day10)
  runDay(Day11)
  runDay(Day12)
  runDay(Day13)
  runDay(Day14)
  runDay(Day15)
  runDay(Day16)
  runDay(Day17)
  runDay(Day18)
  runDay(Day19)
  runDay(Day20)
  runDay(Day21)
  runDay(Day22)
  runDay(Day23)
  runDay(Day24)
  runDay(Day25)
