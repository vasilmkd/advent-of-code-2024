import scala.annotation.tailrec
import scala.io.Source

object Day20:
  type Grid = Array[Array[Char]]

  def readInput(): Grid =
    Source.fromResource("day20.txt").getLines().map(_.toCharArray).toArray

  private def findChar(grid: Grid, c: Char): (Int, Int) =
    var i = 0
    while i < grid.length do
      var j = 0
      while j < grid(i).length do
        if grid(i)(j) == c then return (i, j)
        j += 1
      i += 1
    throw IllegalArgumentException(s"Char $c not found")

  private def path(grid: Grid): Map[(Int, Int), Int] =
    val (ex, ey) = findChar(grid, 'E')
    @tailrec
    def loop(x: Int, y: Int, score: Int, acc: Map[(Int, Int), Int]): Map[(Int, Int), Int] =
      if x == ex && y == ey then acc + ((x, y) -> score)
      else
        val (nx, ny) =
          if x > 0 && grid(x - 1)(y) != '#' && !acc.contains((x - 1, y)) then (x - 1, y)
          else if x < grid.length - 1 && grid(x + 1)(y) != '#' && !acc.contains((x + 1, y)) then (x + 1, y)
          else if y > 0 && grid(x)(y - 1) != '#' && !acc.contains((x, y - 1)) then (x, y - 1)
          else if y < grid(x).length - 1 && grid(x)(y + 1) != '#' && !acc.contains((x, y + 1)) then (x, y + 1)
          else throw IllegalStateException(s"No next for ($x, $y)")
        loop(nx, ny, score + 1, acc + ((x, y) -> score))

    val (sx, sy) = findChar(grid, 'S')
    loop(sx, sy, 0, Map.empty)

  private inline def canCheat1(x1: Int, y1: Int, x2: Int, y2: Int): Boolean =
    val dx = math.abs(x2 - x1)
    val dy = math.abs(y2 - y1)
    dx + dy == 2

  private inline def improvement1(path: Map[(Int, Int), Int], x1: Int, y1: Int, x2: Int, y2: Int): Int =
    val res1 = path((x1, y1))
    val res2 = path((x2, y2))
    val d = math.abs(res1 - res2)
    d - 2

  def part1(): Int =
    val grid = readInput()
    val x = path(grid)
    val coordinates = x.keys.toVector
    val coordinatesX = coordinates.map(_._1).toArray[Int]
    val coordinatesY = coordinates.map(_._2).toArray[Int]
    val summary = new Array[Int](10_000)
    var i1 = 0
    while i1 < coordinatesX.length do
      val x1 = coordinatesX(i1)
      val y1 = coordinatesY(i1)
      var i2 = 0
      while i2 < coordinatesX.length do
        val x2 = coordinatesX(i2)
        val y2 = coordinatesY(i2)
        if !(x1 == x2 && y1 == y2) && canCheat1(x1, y1, x2, y2) then
          val d = improvement1(x, x1, y1, x2, y2)
          if d > 0 then
            val old = summary(d)
            summary(d) = old + 1
        i2 += 1
      i1 += 1

    summary.drop(100).sum / 2

  def answer1: Int = 1411

  enum CanCheat:
    case Yep(cost: Int)
    case Nope

  private inline def canCheat2(x1: Int, y1: Int, x2: Int, y2: Int): CanCheat =
    val dx = math.abs(x2 - x1)
    val dy = math.abs(y2 - y1)
    val d = dx + dy
    if d > 0 && d <= 20 then CanCheat.Yep(d) else CanCheat.Nope

  private inline def improvement2(pathMap: Map[(Int, Int), Int], cost: Int, x1: Int, y1: Int, x2: Int, y2: Int): Int =
    val res1 = pathMap((x1, y1))
    val res2 = pathMap((x2, y2))
    val d = math.abs(res1 - res2)
    d - cost

  def part2(): Int =
    val grid = readInput()
    val x = path(grid)
    val coordinates = x.keys.toVector
    val coordinatesX = coordinates.map(_._1).toArray[Int]
    val coordinatesY = coordinates.map(_._2).toArray[Int]
    val summary = new Array[Int](10_000)
    var i1 = 0
    while i1 < coordinatesX.length do
      val x1 = coordinatesX(i1)
      val y1 = coordinatesY(i1)
      var i2 = 0
      while i2 < coordinatesY.length do
        val x2 = coordinatesX(i2)
        val y2 = coordinatesY(i2)
        if !(x1 == x2 && y1 == y2) then
          canCheat2(x1, y1, x2, y2) match
            case CanCheat.Nope =>
            case CanCheat.Yep(cost) =>
              val d = improvement2(x, cost, x1, y1, x2, y2)
              val old = summary(d)
              summary(d) = old + 1
        i2 += 1
      i1 += 1

    summary.drop(100).sum / 2

  def answer2: Int = 1010263
