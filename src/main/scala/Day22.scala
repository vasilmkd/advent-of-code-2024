import scala.collection.mutable
import scala.io.Source

object Day22:
  def readInput(): LazyList[Long] =
    Source
      .fromResource("day22.txt")
      .getLines()
      .map(_.toLong)
      .to(LazyList)

  private inline def mixIn(x: Long, y: Long): Long = x ^ y

  private inline def prune(x: Long): Long = x % 16777216

  private inline def step1(n: Long): Long =
    val a = n * 64
    val b = mixIn(n, a)
    prune(b)

  private inline def step2(n: Long): Long =
    val a = n / 32
    val b = mixIn(n, a)
    prune(b)

  private inline def step3(n: Long): Long =
    val a = n * 2048
    val b = mixIn(n, a)
    prune(b)

  private inline def step(n: Long): Long = step3(step2(step1(n)))

  case class Vec4(a: Long, b: Long, c: Long, d: Long)

  object Vec4:
    def empty: Vec4 = Vec4(Long.MinValue, Long.MinValue, Long.MinValue, Long.MinValue)

  def part1(): Long =
    val input = readInput()
    input.map: start =>
      LazyList.iterate(start)(step).take(2001).last
    .sum

  def answer1: Long = 12759339434L

  def part2(): Long =
    val input = readInput()

    val sequences = mutable.Map.empty[(Long, Int), mutable.Map[Vec4, Long]]

    input.zipWithIndex.foreach: (start, index) =>
      val seq = LazyList.iterate(start)(step).take(2001)
      sequences((start, index)) = mutable.Map.empty[Vec4, Long]
      seq.foldLeft((Vec4.empty, Long.MinValue)):
        case ((Vec4(Long.MinValue, Long.MinValue, Long.MinValue, Long.MinValue), Long.MinValue), curr) =>
          (Vec4(Long.MinValue, Long.MinValue, Long.MinValue, Long.MinValue), curr % 10)
        case ((Vec4(Long.MinValue, Long.MinValue, Long.MinValue, Long.MinValue), p), curr) =>
          val m = curr % 10
          (Vec4(a = m - p, Long.MinValue, Long.MinValue, Long.MinValue), m)
        case ((Vec4(a, Long.MinValue, Long.MinValue, Long.MinValue), p), curr) =>
          val m = curr % 10
          (Vec4(a = a, b = m - p, Long.MinValue, Long.MinValue), m)
        case ((Vec4(a, b, Long.MinValue, Long.MinValue), p), curr) =>
          val m = curr % 10
          (Vec4(a = a, b = b, c = m - p, Long.MinValue), m)
        case ((Vec4(a, b, c, Long.MinValue), p), curr) =>
          val m = curr % 10
          (Vec4(a = a, b = b, c = c, d = m - p), m)
        case ((v @ Vec4(_, b, c, d), p), curr) =>
          val mapping = sequences((start, index))
          if !mapping.contains(v) then
            mapping(v) = p
          val m = curr % 10
          (Vec4(a = b, b = c, c = d, d = m - p), m)

    val summed = sequences.values.foldLeft(Map.empty[Vec4, Long]):
      case (acc, mapping) =>
        mapping.foldLeft(acc):
          case (acc, (vec, value)) =>
            val old = acc.getOrElse(vec, 0L)
            acc.updated(vec, old + value)
    summed.values.max

  def answer2: Long = 1405L
