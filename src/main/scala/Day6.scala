import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.LongAdder
import scala.annotation.{tailrec, targetName, unused}
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break

object Day6:
  type Grid = IArray[IArray[Char]]

  def readInput(): Grid =
    val array = Source.fromResource("day6.txt")
      .getLines()
      .map(line => IArray.unsafeFromArray(line.toCharArray))
      .toArray
    IArray.unsafeFromArray(array)

  enum Direction:
    case Up, Down, Left, Right

    inline def turn: Direction = this match
      case Up => Right
      case Right => Down
      case Down => Left
      case Left => Up

  //noinspection ScalaUnusedSymbol
  case class Position(x: Int, y: Int, direction: Direction):
    inline def coordinates: (Int, Int) = (x, y)
    @targetName("hash") inline def ### : Int = x * 1000 + y

    inline def before: Position = direction match
      case Direction.Up => Position(x + 1, y, Direction.Up)
      case Direction.Down => Position(x - 1, y, Direction.Down)
      case Direction.Left => Position(x, y + 1, Direction.Left)
      case Direction.Right => Position(x, y - 1, Direction.Right)

  private inline def outOfBounds(g: Grid, p: Position): Boolean =
    p.x < 0 || p.x >= g.length || p.y < 0 || p.y >= g(0).length

  private inline def free(g: Grid, p: Position): Boolean =
    g.apply(p.x).apply(p.y) != '#'

  @tailrec
  private def move(grid: Grid, position: Position): Option[Position] =
    val Position(x, y, direction) = position
    if outOfBounds(grid, position)
    then return None
    val front = direction match
      case Direction.Up => Position(x - 1, y, Direction.Up)
      case Direction.Down => Position(x + 1, y, Direction.Down)
      case Direction.Left => Position(x, y - 1, Direction.Left)
      case Direction.Right => Position(x, y + 1, Direction.Right)
    if outOfBounds(grid, front)
    then return None
    if free(grid, front)
    then Some(front)
    else move(grid, Position(x, y, direction.turn))

  private inline def startPosition(grid: Grid): Position =
    boundary:
      for
        x <- grid.indices
        y <- grid.apply(x).indices
      do
        if grid.apply(x).apply(y) == '^' then
          break(Position(x, y, Direction.Up))
      throw new IllegalArgumentException("No start position found")

  extension (@unused i: Iterator.type) private def unroll[A](start: A)(f: A => Option[A]): Iterator[A] =
    new Iterator[A]:
      private var current: Option[A] = Some(start)
      override def hasNext: Boolean = current.nonEmpty
      override def next(): A =
        val res = current.get
        current = f(res)
        res

  private inline def walk(grid: Grid, start: Position): Iterator[Position] =
    Iterator.unroll(start)(move(grid, _))

  def part1(): Int =
    val grid = readInput()
    val start = startPosition(grid)
    walk(grid, start).map(_.coordinates).toSet.size

  def answer1: Int = 5086

  private inline def modified(grid: Grid, x: Int, y: Int): Grid =
    val array = IArray.genericWrapArray(grid).toArray
    val line = IArray.genericWrapArray(array(x)).toArray
    line(y) = '#'
    array(x) = IArray.unsafeFromArray(line)
    IArray.unsafeFromArray(array)

  private def isLoop(grid: Grid, start: Position): Boolean =
    val iterator = walk(grid, start)
    val seen = mutable.Set.empty[Position]
    while iterator.hasNext do
      val current = iterator.next()
      if seen.contains(current) then return true
      seen += current
    false

  private inline def canModify(grid: Grid, x: Int, y: Int): Boolean =
    grid.apply(x).apply(y) == '.'

  extension [A](as: Iterable[A]) def parCount(f: A => Boolean): Int =
    import scala.concurrent.ExecutionContext.Implicits.global
    val adder = new LongAdder()
    val latch = new CountDownLatch(as.size)
    as.foreach: a =>
      Future:
        if f(a) then adder.increment()
        latch.countDown()
    latch.await()
    adder.intValue()

  def part2(): Int =
    val grid = readInput()
    val start = startPosition(grid)
    val positions = walk(grid, start).to(ArraySeq).distinctBy(_.###)
    positions.parCount(p => canModify(grid, p.x, p.y) && isLoop(modified(grid, p.x, p.y), p.before))

  def answer2: Int = 1770
