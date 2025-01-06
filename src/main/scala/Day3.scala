import scala.io.Source
import scala.util.matching.Regex

object Day3:
  def readInput(): String =
    Source.fromResource("day3.txt").getLines().mkString

  private val mulPattern: Regex = """mul\((\d+),(\d+)\)""".r
  private val doPattern: Regex = """do\(\)""".r
  private val dontPattern: Regex = """don't\(\)""".r

  private val combined: Regex = s"${mulPattern.regex}|${doPattern.regex}|${dontPattern.regex}".r

  def part1(): Int =
    val input = readInput()
    mulPattern.findAllMatchIn(input).map:
      case mulPattern(x, y) => x.toInt * y.toInt
    .sum

  def answer1: Int = 189527826

  case class ComputerState(accumulator: Int, mulEnabled: Boolean)

  def part2(): Int =
    val input = readInput()
    combined.findAllMatchIn(input).foldLeft(ComputerState(0, true)):
      case (ComputerState(acc, true), mulPattern(x, y)) => ComputerState(acc + x.toInt * y.toInt, true)
      case (ComputerState(acc, false), doPattern()) => ComputerState(acc, true)
      case (ComputerState(acc, true), dontPattern()) => ComputerState(acc, false)
      case (state, _) => state
    .accumulator

  def answer2: Int = 63013756
