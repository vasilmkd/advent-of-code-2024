import java.util.concurrent.CountDownLatch
import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Source

object Day16:
  type Grid = Array[Array[Char]]

  def readInput(): Grid =
    Source.fromResource("day16.txt").getLines().map(_.toCharArray).toArray

  private def findChar(grid: Grid, char: Char): (Int, Int) =
    var x = 0
    while x < grid.length do
      var y = 0
      while y < grid(x).length do
        if grid(x)(y) == char then
          return (x, y)
        y += 1
      x += 1
    throw new IllegalArgumentException(s"Not found: $char")

  enum Direction:
    case Up, Down, Left, Right

  case class State(coord: (Int, Int), direction: Direction, score: Int, path: Vector[(Int, Int)]):
    def inBounds(grid: Grid): Boolean =
      val (x, y) = coord
      x >= 0 && x < grid.length && y >= 0 && y < grid(x).length

  private def bfs(grid: Grid, sx: Int, sy: Int, ex: Int, ey: Int): Option[State] =
    given Ordering[State] = Ordering.by[State, Int](_.score).reverse
    val startCoord = sx -> sy
    val queue = mutable.PriorityQueue(State(startCoord, Direction.Right, 0, Vector(startCoord)))
    val seen = mutable.Set.empty[(Int, Int)]
    while queue.nonEmpty do
      val state @ State(coord @ (x, y), direction, score, path) = queue.dequeue()
      if x == ex && y == ey then return Some(state)
      else if !seen.contains(coord) then
        seen += coord
        val front = direction match
          case Direction.Up =>
            val next = (x - 1, y)
            State(next, direction, score + 1, path :+ next)
          case Direction.Down =>
            val next = (x + 1, y)
            State(next, direction, score + 1, path :+ next)
          case Direction.Left =>
            val next = (x, y - 1)
            State(next, direction, score + 1, path :+ next)
          case Direction.Right =>
            val next = (x, y + 1)
            State(next, direction, score + 1, path :+ next)
        if front.inBounds(grid) && grid(front.coord._1)(front.coord._2) != '#' then
          queue += front
        val left = direction match
          case Direction.Up =>
            val next = (x, y - 1)
            State(next, Direction.Left, score + 1001, path :+ next)
          case Direction.Down =>
            val next = (x, y + 1)
            State(next, Direction.Right, score + 1001, path :+ next)
          case Direction.Left =>
            val next = (x + 1, y)
            State(next, Direction.Down, score + 1001, path :+ next)
          case Direction.Right =>
            val next = (x - 1, y)
            State(next, Direction.Up, score + 1001, path :+ next)
        if left.inBounds(grid) && grid(left.coord._1)(left.coord._2) != '#' then
          queue += left
        val right = direction match
          case Direction.Up =>
            val next = (x, y + 1)
            State(next, Direction.Right, score + 1001, path :+ next)
          case Direction.Down =>
            val next = (x, y - 1)
            State(next, Direction.Left, score + 1001, path :+ next)
          case Direction.Left =>
            val next = (x - 1, y)
            State(next, Direction.Up, score + 1001, path :+ next)
          case Direction.Right =>
            val next = (x + 1, y)
            State(next, Direction.Down, score + 1001, path :+ next)
        if right.inBounds(grid) && grid(right.coord._1)(right.coord._2) != '#' then
          queue += right
    None

  private inline def cloneGrid(grid: Grid): Grid =
    val res = Array.ofDim[Char](grid.length, grid(0).length)
    var i = 0
    while i < grid.length do
      System.arraycopy(grid(i), 0, res(i), 0, grid(i).length)
      i += 1
    res

  def part1(): Int =
    val input = readInput()
    val (sx, sy) = findChar(input, 'S')
    val (ex, ey) = findChar(input, 'E')
    bfs(input, sx, sy, ex, ey).get.score

  def answer1: Int = 99448

  def part2(): Int =
    val input = readInput()
    val (sx, sy) = findChar(input, 'S')
    val (ex, ey) = findChar(input, 'E')
    val State(_, _, score, path) =
      bfs(input, sx, sy, ex, ey).getOrElse(throw IllegalArgumentException("No path found"))

    val seats = TrieMap.empty[(Int, Int), Boolean]
    val latch = CountDownLatch(path.size)
    path.foreach: (x, y) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      Future:
        val grid = cloneGrid(input)
        grid(x)(y) = '#'
        bfs(grid, sx, sy, ex, ey) match
          case Some(State(_, _, s, p)) if s == score =>
            p.foreach(seats.put(_, true))
          case _ =>
        latch.countDown()
    latch.await()
    seats.size

  def answer2: Int = 498
