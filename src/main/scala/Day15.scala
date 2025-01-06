import scala.collection.mutable
import scala.io.Source

object Day15:
  type Grid = Array[Array[Char]]

  def readInput(): (Grid, String) =
    val lines =
      Source
        .fromResource("day15.txt")
        .getLines()
        .toVector

    val empty = lines.indexOf("")
    val (gLines, mLines) = lines.splitAt(empty)
    val grid = gLines.map(_.toCharArray).toArray
    val moves = mLines.mkString
    (grid, moves)

  type Direction = '^' | 'v' | '<' | '>'

  private def move1(grid: Grid, x: Int, y: Int, direction: Direction): (Grid, Int, Int) = direction match
    case '^' => moveUp1(grid, x, y)
    case 'v' => moveDown1(grid, x, y)
    case '<' => moveLeft1(grid, x, y)
    case '>' => moveRight1(grid, x, y)

  private def moveUp1(grid: Grid, x: Int, y: Int): (Grid, Int, Int) =
    var i = x - 1
    while i >= 0 && grid(i)(y) == 'O' do
      i -= 1
    if grid(i)(y) == '#' then return (grid, x, y)
    grid(i)(y) = 'O'
    grid(x - 1)(y) = '@'
    grid(x)(y) = '.'
    (grid, x - 1, y)

  private def moveDown1(grid: Grid, x: Int, y: Int): (Grid, Int, Int) =
    var i = x + 1
    while i < grid.length && grid(i)(y) == 'O' do
      i += 1
    if grid(i)(y) == '#' then return (grid, x, y)
    grid(i)(y) = 'O'
    grid(x + 1)(y) = '@'
    grid(x)(y) = '.'
    (grid, x + 1, y)

  private def moveLeft1(grid: Grid, x: Int, y: Int): (Grid, Int, Int) =
    var i = y - 1
    while i >= 0 && grid(x)(i) == 'O' do
      i -= 1
    if grid(x)(i) == '#' then return (grid, x, y)
    grid(x)(i) = 'O'
    grid(x)(y - 1) = '@'
    grid(x)(y) = '.'
    (grid, x, y - 1)

  private def moveRight1(grid: Grid, x: Int, y: Int): (Grid, Int, Int) =
    var i = y + 1
    while i < grid(x).length && grid(x)(i) == 'O' do
      i += 1
    if grid(x)(i) == '#' then return (grid, x, y)
    grid(x)(i) = 'O'
    grid(x)(y + 1) = '@'
    grid(x)(y) = '.'
    (grid, x, y + 1)

  private def start(grid: Grid): (Int, Int) =
    var x = 0
    while x < grid.length do
      var y = 0
      while y < grid(x).length do
        if grid(x)(y) == '@' then return (x, y)
        y += 1
      x += 1
    throw new IllegalArgumentException("No starting point found")

  extension (c: Char) private def toDirection: Direction = c match
    case '<' => '<'.asInstanceOf[Direction]
    case '>' => '>'.asInstanceOf[Direction]
    case 'v' => 'v'.asInstanceOf[Direction]
    case '^' => '^'.asInstanceOf[Direction]
    case _ => throw new IllegalArgumentException(s"Invalid direction: $c")

  private def boxCoordinatesSum1(grid: Grid): Int =
    var sum = 0
    var x = 0
    while x < grid.length do
      var y = 0
      while y < grid(x).length do
        if grid(x)(y) == 'O' then sum += 100 * x + y
        y += 1
      x += 1
    sum

  def part1(): Int =
    val (grid, moves) = readInput()
    val (sx, sy) = start(grid)
    val (finalGrid, _, _) = moves.foldLeft((grid, sx, sy)):
      case ((g, x, y), c) => move1(g, x, y, c.toDirection)
    boxCoordinatesSum1(finalGrid)

  def answer1: Int = 1442192

  private def scale(grid: Grid): Grid =
    val res = Array.ofDim[Char](grid.length, 2 * grid(0).length)
    var x = 0
    while x < grid.length do
      var y = 0
      while y < grid(x).length do
        val c = grid(x)(y)
        if c == '#' then
          res(x)(2 * y) = '#'
          res(x)(2 * y + 1) = '#'
        else if c == 'O' then
          res(x)(2 * y) = '['
          res(x)(2 * y + 1) = ']'
        else if c == '.' then
          res(x)(2 * y) = '.'
          res(x)(2 * y + 1) = '.'
        else if c == '@' then
          res(x)(2 * y) = '@'
          res(x)(2 * y + 1) = '.'
        y += 1
      x += 1
    res

  private def move2(grid: Grid, x: Int, y: Int, direction: Direction): (Grid, Int, Int) = direction match
    case '^' => moveUp2(grid, x, y)
    case 'v' => moveDown2(grid, x, y)
    case '<' => moveLeft2(grid, x, y)
    case '>' => moveRight2(grid, x, y)

  private def moveUp2(grid: Grid, x: Int, y: Int): (Grid, Int, Int) =
    val affectedCoordinates = affectedUp(grid, x, y)
    val can = (affectedCoordinates + (x -> y)).forall((a, b) => canMoveUp(grid, a, b))
    if !can then return (grid, x, y)
    val sorted = affectedCoordinates.toVector.sortBy((x, y) => 1000 * x + y)
    sorted.foreach: (x, y) =>
      grid(x - 1)(y) = grid(x)(y)
      grid(x)(y) = '.'
    grid(x - 1)(y) = '@'
    grid(x)(y) = '.'
    (grid, x - 1, y)

  private def affectedUp(grid: Grid, x: Int, y: Int): Set[(Int, Int)] =
    if x == 0 then return Set.empty
    val queue = mutable.Queue((x - 1) -> y)
    val res = Set.newBuilder[(Int, Int)]
    while queue.nonEmpty do
      val (i, j) = queue.dequeue()
      val c = grid(i)(j)
      if c == '[' then
        res += (i -> j)
        res += (i -> (j + 1))
        queue += ((i - 1) -> j)
        queue += ((i - 1) -> (j + 1))
      else if c == ']' then
        res += (i -> (j - 1))
        res += (i -> j)
        queue += ((i - 1) -> (j - 1))
        queue += ((i - 1) -> j)
    res.result()

  private def canMoveUp(grid: Grid, x: Int, y: Int): Boolean =
    if x == 0 then return false
    if grid(x - 1)(y) == '#' then return false
    true

  private def moveDown2(grid: Grid, x: Int, y: Int): (Grid, Int, Int) =
    val affectedCoordinates = affectedDown(grid, x, y)
    val can = (affectedCoordinates + (x -> y)).forall((a, b) => canMoveDown(grid, a, b))
    if !can then return (grid, x, y)
    val sorted = affectedCoordinates.toVector.sortBy((x, y) => 1000 * x + y).reverse
    sorted.foreach: (x, y) =>
      grid(x + 1)(y) = grid(x)(y)
      grid(x)(y) = '.'
    grid(x + 1)(y) = '@'
    grid(x)(y) = '.'
    (grid, x + 1, y)

  private def affectedDown(grid: Grid, x: Int, y: Int): Set[(Int, Int)] =
    if x == grid.length - 1 then return Set.empty
    val queue = mutable.Queue((x + 1) -> y)
    val res = Set.newBuilder[(Int, Int)]
    while queue.nonEmpty do
      val (i, j) = queue.dequeue()
      val c = grid(i)(j)
      if c == '[' then
        res += (i -> j)
        res += (i -> (j + 1))
        queue += ((i + 1) -> j)
        queue += ((i + 1) -> (j + 1))
      else if c == ']' then
        res += (i -> (j - 1))
        res += (i -> j)
        queue += ((i + 1) -> (j - 1))
        queue += ((i + 1) -> j)
    res.result()

  private def canMoveDown(grid: Grid, x: Int, y: Int): Boolean =
    if x == grid.length - 1 then return false
    if grid(x + 1)(y) == '#' then return false
    true

  private def moveLeft2(grid: Grid, x: Int, y: Int): (Grid, Int, Int) =
    val affectedCoordinates = affectedLeft(grid, x, y)
    val can = (affectedCoordinates + (x -> y)).forall((a, b) => canMoveLeft(grid, a, b))
    if !can then return (grid, x, y)
    val sorted = affectedCoordinates.toVector.sortBy((x, y) => 1000 * x + y)
    sorted.foreach: (x, y) =>
      grid(x)(y - 1) = grid(x)(y)
      grid(x)(y) = '.'
    grid(x)(y - 1) = '@'
    grid(x)(y) = '.'
    (grid, x, y - 1)

  private def affectedLeft(grid: Grid, x: Int, y: Int): Set[(Int, Int)] =
    if y == 0 then return Set.empty
    val queue = mutable.Queue(x -> (y - 1))
    val res = Set.newBuilder[(Int, Int)]
    while queue.nonEmpty do
      val (i, j) = queue.dequeue()
      val c = grid(i)(j)
      if c == ']' then
        res += (i -> (j - 1))
        res += (i -> j)
        queue += (i -> (j - 2))
    res.result()

  private def canMoveLeft(grid: Grid, x: Int, y: Int): Boolean =
    if y == 0 then return false
    if grid(x)(y - 1) == '#' then return false
    true

  private def moveRight2(grid: Grid, x: Int, y: Int): (Grid, Int, Int) =
    val affectedCoordinates = affectedRight(grid, x, y)
    val can = (affectedCoordinates + (x -> y)).forall((a, b) => canMoveRight(grid, a, b))
    if !can then return (grid, x, y)
    val sorted = affectedCoordinates.toVector.sortBy((x, y) => 1000 * x + y).reverse
    sorted.foreach: (x, y) =>
      grid(x)(y + 1) = grid(x)(y)
      grid(x)(y) = '.'
    grid(x)(y + 1) = '@'
    grid(x)(y) = '.'
    (grid, x, y + 1)

  private def affectedRight(grid: Grid, x: Int, y: Int): Set[(Int, Int)] =
    if y == grid(x).length - 1 then return Set.empty
    val queue = mutable.Queue(x -> (y + 1))
    val res = Set.newBuilder[(Int, Int)]
    while queue.nonEmpty do
      val (i, j) = queue.dequeue()
      val c = grid(i)(j)
      if c == '[' then
        res += (i -> j)
        res += (i -> (j + 1))
        queue += (i -> (j + 2))
    res.result()

  private def canMoveRight(grid: Grid, x: Int, y: Int): Boolean =
    if y == grid(x).length - 1 then return false
    if grid(x)(y + 1) == '#' then return false
    true

  private def boxCoordinatesSum2(grid: Grid): Int =
    var sum = 0
    var x = 0
    while x < grid.length do
      var y = 0
      while y < grid(x).length do
        if grid(x)(y) == '[' then sum += 100 * x + y
        y += 1
      x += 1
    sum

  def part2(): Int =
    val (grid, moves) = readInput()
    val scaled = scale(grid)
    val (sx, sy) = start(scaled)
    val (finalGrid, _, _) = moves.foldLeft((scaled, sx, sy)):
      case ((g, x, y), c) => move2(g, x, y, c.toDirection)
    boxCoordinatesSum2(finalGrid)

  def answer2: Int = 1448458
