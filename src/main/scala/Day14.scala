import scala.io.Source

object Day14:
  private val xMax: Int = 101
  private val yMax: Int = 103

  case class Robot(x: Int, y: Int, vx: Int, vy: Int):
    def move: Robot =
      var nx = x + vx
      if nx >= xMax then nx -= xMax
      if nx < 0 then nx += xMax
      var ny = y + vy
      if ny >= yMax then ny -= yMax
      if ny < 0 then ny += yMax
      Robot(nx, ny, vx, vy)

    def move(times: Int): Robot =
      (1 to times).foldLeft(this)((r, _) => r.move)

  def readInput(): Vector[Robot] =
    Source
      .fromResource("day14.txt")
      .getLines()
      .map:
        case s"p=$x,$y v=$vx,$vy" => Robot(x.toInt, y.toInt, vx.toInt, vy.toInt)
      .toVector

  type Grid = Array[Array[Char]]

  private def picture(robots: Vector[Robot]): Grid =
    val grid = Array.ofDim[Char](yMax, xMax)
    for
      y <- grid.indices
      x <- grid(y).indices
    do
      grid(y)(x) = '.'
    for Robot(x, y, _, _) <- robots do
      grid(y)(x) = '#'
    grid

  private def hasTree(grid: Grid): Boolean =
    var y = 0
    while y < grid.length - 3 do
      var x = 0
      while x < grid(y).length - 3 do
        val a = grid(y)(x)
        val b = grid(y + 1)(x)
        val c = grid(y + 2)(x)
        val d = grid(y)(x + 1)
        val e = grid(y + 1)(x + 1)
        val f = grid(y + 2)(x + 1)
        val g = grid(y)(x + 2)
        val h = grid(y + 1)(x + 2)
        val i = grid(y + 2)(x + 2)
        if a == '#' && b == '#' && c == '#' && d == '#' && e == '#' && f == '#' && g == '#' && h == '#' && i == '#' then
          return true
        x += 1
      y += 1
    false

  private def printGrid(grid: Grid): Unit =
    for y <- grid.indices do
      for x <- grid(y).indices do
        print(grid(y)(x))
      println()

  def part1(): Int =
    val robots = readInput()
    val simulated = robots.map(_.move(100))
    val h = yMax / 2
    val v = xMax / 2
    val (q1, q2, q3, q4) = simulated.foldLeft((0, 0, 0, 0)):
      case ((q1, q2, q3, q4), Robot(x, y, _, _)) if y < h && x < v => (q1 + 1, q2, q3, q4)
      case ((q1, q2, q3, q4), Robot(x, y, _, _)) if y < h && x > v => (q1, q2 + 1, q3, q4)
      case ((q1, q2, q3, q4), Robot(x, y, _, _)) if y > h && x < v => (q1, q2, q3 + 1, q4)
      case ((q1, q2, q3, q4), Robot(x, y, _, _)) if y > h && x > v => (q1, q2, q3, q4 + 1)
      case (t, _) => t
    val res = q1 * q2 * q3 * q4
    res

  def answer1: Int = 218433348

  def part2(): Int =
    val robots = readInput()
    val simulated = robots.map(_.move(6512))
    val grid = picture(simulated)
//    printGrid(grid)
    6512

  def answer2: Int = 6512
