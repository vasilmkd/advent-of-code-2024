import scala.io.Source

object Day1:
  def readInput(): (Array[Int], Array[Int]) =
    def parseLine(line: String): (Int, Int) = line match
      case s"$x   $y" => (x.toInt, y.toInt)
      case _ => throw new IllegalArgumentException(s"Invalid line: $line")

    val (xs, ys) = Source
      .fromResource("day1.txt")
      .getLines()
      .map(parseLine)
      .foldLeft((Array.newBuilder[Int], Array.newBuilder[Int])):
        case ((xs, ys), (x, y)) => (xs += x, ys += y)
    (xs.result(), ys.result())

  def part1(): Int =
    extension (as: Array[Int]) def sortedInPlace: as.type =
      java.util.Arrays.sort(as)
      as

    val (xs, ys) = readInput()
    xs.sortedInPlace.zip(ys.sortedInPlace).foldLeft(0):
      case (acc, (x, y)) => acc + math.abs(x - y)

  def answer1: Int = 1889772

  def part2(): Int =
    val (xs, ys) = readInput()
    val counts = ys.foldLeft(Map.empty[Int, Int]): (acc, y) =>
      acc.updated(y, acc.getOrElse(y, 0) + 1)
    xs.foldLeft(0): (acc, x) =>
      acc + x * counts.getOrElse(x, 0)

  def answer2: Int = 23228917
