import scala.io.Source

object Day4:
  extension [A](array: Array[A]) private def toIArray: IArray[A] = IArray.unsafeFromArray(array)
  
  private type Crossword = IArray[IArray[Char]]
  
  def readInput(): Crossword =
    Source.fromResource("day4.txt").getLines().map(_.toCharArray.toIArray).toArray.toIArray
    
  private val word: IArray[Char] = "XMAS".toCharArray.toIArray

  enum Direction(val offsets: IArray[(Int, Int)]):
    case UpLeft extends Direction(IArray((0, 0), (-1, -1), (-2, -2), (-3, -3)))
    case Up extends Direction(IArray((0, 0), (-1, 0), (-2, 0), (-3, 0)))
    case UpRight extends Direction(IArray((0, 0), (-1, 1), (-2, 2), (-3, 3)))
    case Right extends Direction(IArray((0, 0), (0, 1), (0, 2), (0, 3)))
    case DownRight extends Direction(IArray((0, 0), (1, 1), (2, 2), (3, 3)))
    case Down extends Direction(IArray((0, 0), (1, 0), (2, 0), (3, 0)))
    case DownLeft extends Direction(IArray((0, 0), (1, -1), (2, -2), (3, -3)))
    case Left extends Direction(IArray((0, 0), (0, -1), (0, -2), (0, -3)))

  private inline def withinBounds(crossword: Crossword, x: Int, y: Int): Boolean =
    0 <= x && x < crossword.length && 0 <= y && y < crossword(x).length
  
  private inline def applyOffsets(crossword: Crossword, offsets: IArray[(Int, Int)], x: Int, y: Int): IArray[(Int, Int)] =
    offsets.map((ox, oy) => (x + ox, y + oy)).filter(withinBounds(crossword, _, _))
    
  private inline def searchDirection(crossword: Crossword, direction: Direction, x: Int, y: Int): Boolean =
    applyOffsets(crossword, direction.offsets, x, y).map((x, y) => crossword.apply(x).apply(y)).sameElements(word)

  private inline def searchAllDirections(crossword: Crossword, x: Int, y: Int): Int =
    Direction.values.count(searchDirection(crossword, _, x, y))

  private inline def searchCrossword(crossword: Crossword): Int =
    crossword.indices.flatMap: x =>
      crossword.apply(x).indices.map: y =>
        searchAllDirections(crossword, x, y)
    .sum

  def part1(): Int =
    searchCrossword(readInput())

  def answer1: Int = 2560

  private enum Cross(val grid: IArray[IArray[Char]]):
    case One extends Cross(IArray(IArray('M', '*', 'M'), IArray('*', 'A', '*'), IArray('S', '*', 'S')))
    case Two extends Cross(IArray(IArray('M', '*', 'S'), IArray('*', 'A', '*'), IArray('M', '*', 'S')))
    case Three extends Cross(IArray(IArray('S', '*', 'M'), IArray('*', 'A', '*'), IArray('S', '*', 'M')))
    case Four extends Cross(IArray(IArray('S', '*', 'S'), IArray('*', 'A', '*'), IArray('M', '*', 'M')))

  private inline def extractGrid(crossword: Crossword, x: Int, y: Int): IArray[IArray[Char]] =
    IArray(
      IArray[Char](crossword(x - 1).apply(y - 1), crossword(x - 1).apply(y), crossword(x - 1).apply(y + 1)),
      IArray[Char](crossword(x).apply(y - 1), crossword(x).apply(y), crossword(x).apply(y + 1)),
      IArray[Char](crossword(x + 1).apply(y - 1), crossword(x + 1).apply(y), crossword(x + 1).apply(y + 1))
    )

  private inline def compareCrosses(cross: Cross, grid: IArray[IArray[Char]]): Boolean =
    cross.grid.indices.forall: x =>
      cross.grid.apply(x).indices.forall: y =>
        val c1 = cross.grid.apply(x).apply(y)
        val c2 = grid.apply(x).apply(y)
        c1 == '*' || c1 == c2

  private inline def countCross(cross: Cross, grid: IArray[IArray[Char]]): Int =
    if compareCrosses(cross, grid) then 1 else 0

  private inline def searchCrosses(crossword: Crossword): Int =
    crossword.indices.drop(1).dropRight(1).flatMap: x =>
      crossword.apply(x).indices.drop(1).dropRight(1).map: y =>
        val grid = extractGrid(crossword, x, y)
        Cross.values.map(countCross(_, grid)).sum
    .sum

  def part2(): Int =
    searchCrosses(readInput())

  def answer2: Int = 1910
