import scala.annotation.tailrec
import scala.io.Source
import scala.collection.mutable

object Day8:
  type Grid = Array[Array[Char]]

  def readInput(): Grid =
    Source
      .fromResource("day8.txt")
      .getLines()
      .map(_.toCharArray)
      .toArray

  private inline def dist(x: Int, y: Int, i: Int, j: Int): (Int, Int) =
    val dx = i - x
    val dy = j - y
    (dx, dy)

  private inline def applyDist(x: Int, y: Int, dx: Int, dy: Int): (Int, Int) = (x + dx, y + dy)

  private inline def inBounds(grid: Grid, x: Int, y: Int): Boolean =
    x >= 0 && x < grid.length && y >= 0 && y < grid(x).length

  private def countAntinodes1(grid: Grid, x: Int, y: Int): Set[(Int, Int)] =
    val antinodes = mutable.Set.empty[(Int, Int)]
    var i = 0
    while i < grid.length do
      var j = 0
      while j < grid(i).length do
        val c = grid(i)(j)
        if (x, y) != (i, j) && c != '.' then
          val (dx, dy) = dist(x, y, i, j)
          val (ni, nj) = applyDist(i, j, dx, dy)
          if inBounds(grid, ni, nj) && grid(ni)(nj) == c then
            antinodes += (x -> y)
        j += 1
      i += 1
    antinodes.toSet

  private inline def scaled(dx: Int, dy: Int, factor: Int): (Int, Int) = (dx * factor, dy * factor)

  @tailrec
  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

//  private inline def lcm(a: Int, b: Int): Int = a * b / gcd(a, b)

  private def countAntinodes2(grid: Grid): Set[(Int, Int)] =
    val nodes = mutable.Set.empty[(Int, Int, Char)]
    var i = 0
    while i < grid.length do
      var j = 0
      while j < grid(i).length do
        val c = grid(i)(j)
        if c != '.' then
          nodes += ((i, j, c))
        j += 1
      i += 1

    val antinodes = mutable.Set.empty[(Int, Int)]
    val nodesArray = nodes.toArray
    var n1 = 0
    while n1 < nodesArray.length do
      var n2 = 0
      while n2 < nodesArray.length do
        val (x1, y1, c1) = nodesArray(n1)
        val (x2, y2, c2) = nodesArray(n2)
        if n1 != n2 && c1 == c2 then
          val (dx, dy) = dist(x1, y1, x2, y2)
          val g = gcd(dx, dy)
          val ndx = dx / g
          val ndy = dy / g
          var factor = -100
          while factor < 100 do
            factor += 1
            val (sdx, sdy) = scaled(ndx, ndy, factor)
            val (x, y) = applyDist(x1, y1, sdx, sdy)
            if inBounds(grid, x, y) then
              antinodes += (x -> y)
        n2 += 1
      n1 += 1
    antinodes.toSet

  def part1(): Int =
    val grid = readInput()
    val antinodes = mutable.Set.empty[(Int, Int)]
    var x = 0
    while x < grid.length do
      var y = 0
      while y < grid(x).length do
        antinodes ++= countAntinodes1(grid, x, y)
        y += 1
      x += 1
    antinodes.size

  def answer1: Int = 271

  def part2(): Int =
    val grid = readInput()
    countAntinodes2(grid).size

  def answer2: Int = 994
