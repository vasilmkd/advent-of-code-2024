import scala.collection.mutable
import scala.io.Source

object Day10:
  type Grid = Array[Array[Char]]

  def readInput(): Grid =
    Source.fromResource("day10.txt").getLines().map(_.toCharArray).toArray

  private def trailheads(grid: Grid): Array[(Int, Int)] =
    val builder = Array.newBuilder[(Int, Int)]
    var x = 0
    while x < grid.length do
      var y = 0
      while y < grid(x).length do
        if grid(x)(y) == '0' then
          builder += (x -> y)
        y += 1
      x += 1
    builder.result()

  private inline def height(grid: Grid, x: Int, y: Int): Int = grid(x)(y) - '0'

  private def score(grid: Grid, trailhead: (Int, Int), rating: Boolean): Int =
    var result = 0
    val queue = mutable.Queue(trailhead)
    val seen = mutable.Set.empty[(Int, Int)]
    while queue.nonEmpty do
      val curr @ (x, y) = queue.dequeue()
      if rating || !seen.contains(curr) then
        seen += curr
        val h = height(grid, x, y)
        if h == 9 then result += 1
        val up = (x - 1, y)
        if x > 0 && !seen.contains(up) && height(grid, x - 1, y) == h + 1 then queue.enqueue(up)
        val down = (x + 1, y)
        if x < grid.length - 1 && !seen.contains(down) && height(grid, x + 1, y) == h + 1 then queue.enqueue(down)
        val left = (x, y - 1)
        if y > 0 && !seen.contains(left) && height(grid, x, y - 1) == h + 1 then queue.enqueue(left)
        val right = (x, y + 1)
        if y < grid(x).length - 1 && !seen.contains(right) && height(grid, x, y + 1) == h + 1 then queue.enqueue(right)
    result

  def part1(): Int =
    val grid = readInput()
    val t = trailheads(grid)
    t.map(score(grid, _, rating = false)).sum

  def answer1: Int = 510

  def part2(): Int =
    val grid = readInput()
    val t = trailheads(grid)
    t.map(score(grid, _, rating = true)).sum

  def answer2: Int = 1058
