import scala.io.Source

object Day13:
  case class RawEquation(line1: String, line2: String, line3: String):
    def toEquation: Equation =
      val (a, b) = line1 match
        case s"Button A: X+$a, Y+$b" => (a.toLong, b.toLong)
      val (c, d) = line2 match
        case s"Button B: X+$c, Y+$d" => (c.toLong, d.toLong)
      val (e, f) = line3 match
        case s"Prize: X=$e, Y=$f" => (e.toLong, f.toLong)
      Equation(a, c, b, d, e, f)

  // ax + by = e, cx + dy = f
  case class Equation(a: Long, b: Long, c: Long, d: Long, e: Long, f: Long):
    def solve: Option[(Long, Long)] =
      val ia = d
      val ib = -b
      val ic = -c
      val id = a
      val coef = a * d - b * c
      val ma = ia * e + ib * f
      val mb = ic * e + id * f
      val X = ma / coef
      val Y = mb / coef
      if a * X + b * Y == e && c * X + d * Y == f then
        Some((X, Y))
      else
        None

  def readInput(): Vector[Equation] =
    val (line1, line2, line3) =
      Source
        .fromResource("day13.txt")
        .getLines()
        .foldLeft((Vector.empty[String], Vector.empty[String], Vector.empty[String])):
          case ((line1, line2, line3), line) if line.startsWith("Button A:") =>
            (line1 :+ line, line2, line3)
          case ((line1, line2, line3), line) if line.startsWith("Button B:") =>
            (line1, line2 :+ line, line3)
          case ((line1, line2, line3), line) if line.startsWith("Prize:") =>
            (line1, line2, line3 :+ line)
          case (t, _) => t
    line1.zip(line2).zip(line3).map:
      case ((l1, l2), l3) => RawEquation(l1, l2, l3).toEquation

  def part1(): Long =
    val input = readInput()
    input.flatMap(_.solve).map:
      case (x, y) => 3 * x + y
    .sum

  def answer1: Long = 28887L

  def part2(): Long =
    val extra = 10_000_000_000_000L
    val input = readInput()
    input.map:
      case Equation(a, b, c, d, e, f) => Equation(a, b, c, d, e + extra, f + extra)
    .flatMap(_.solve)
    .map:
      case (x, y) => 3 * x + y
    .sum

  def answer2: Long = 96979582619758L
