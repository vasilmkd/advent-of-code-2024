import scala.collection.mutable
import scala.io.Source

object Day18:
  type Grid = Array[Array[Char]]

  def readInput(): LazyList[(Int, Int)] =
    Source
      .fromResource("day18.txt")
      .getLines()
      .map:
        case s"$x,$y" => (x.toInt, y.toInt)
      .to(LazyList)

  private def makeGrid(points: LazyList[(Int, Int)]): Grid =
    val grid = Array.ofDim[Char](71, 71)
    var y = 0
    while y < grid.length do
      var x = 0
      while x < grid(y).length do
        grid(x)(y) = '.'
        x += 1
      y += 1
    for (x, y) <- points do
      grid(y)(x) = '#'
    grid

  private def bfs(grid: Grid): Int =
    val queue = mutable.Queue((0, 0, 0))
    val seen = mutable.Set.empty[(Int, Int)]
    while queue.nonEmpty do
      val (x, y, d) = queue.dequeue()
      if y == grid.length - 1 && x == grid(y).length - 1 then return d
      if !seen.contains(x -> y) then
        seen += (x -> y)
        val up = (x, y - 1, d + 1)
        if y > 0 && grid(x)(y - 1) == '.' && !seen.contains(x -> (y - 1)) then queue.enqueue(up)
        val down = (x, y + 1, d + 1)
        if y < grid(x).length - 1 && grid(x)(y + 1) == '.' && !seen.contains(x -> (y + 1)) then queue.enqueue(down)
        val left = (x - 1, y, d + 1)
        if x > 0 && grid(x - 1)(y) == '.' && !seen.contains((x - 1) -> y) then queue.enqueue(left)
        val right = (x + 1, y, d + 1)
        if x < grid.length - 1 && grid(x + 1)(y) == '.' && !seen.contains((x + 1) -> y) then queue.enqueue(right)
    -1

  def part1(): Int =
    val input = readInput()
    val relevant = input.take(1024)
    val grid = makeGrid(relevant)
    bfs(grid)

  def answer1: Int = 290

  def part2(): String =
    val input = readInput()
    val res = (input.size - 1 to 0 by -1).find: i =>
      val grid = makeGrid(input.take(i))
      bfs(grid) != -1
    res match
      case Some(i) =>
        val (x, y) = input(i)
        s"$x,$y"
      case None =>
        throw IllegalArgumentException("No solution found")

  def answer2: String = "64,54"
