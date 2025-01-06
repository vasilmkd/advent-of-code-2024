import scala.io.Source

object Day25:
  type Schematic = Array[Array[Char]]

  def readInput(): Vector[Schematic] =
    val lines = Source.fromResource("day25.txt").getLines().toVector :+ ""
    lines.grouped(8).map: v =>
      v.filter(_.nonEmpty).map(_.toCharArray).toArray
    .toVector

  case class Lock(heights: Vector[Int])

  case class Key(heights: Vector[Int])

  private def parseLock(schematic: Schematic): Option[Lock] =
    if schematic(0)(0) != '#' then return None
    val builder = Vector.newBuilder[Int]
    for
      col <- schematic(0).indices
    do
      var counter = -1
      for
        row <- schematic.indices
      do
        if schematic(row)(col) == '#' then counter += 1
      builder += counter
    Some(Lock(builder.result()))

  private def parseKey(schematic: Schematic): Option[Key] =
    if schematic(0)(0) != '.' then return None
    val builder = Vector.newBuilder[Int]
    for
      col <- schematic(0).indices
    do
      var counter = -1
      for
        row <- schematic.indices.reverse
      do
        if schematic(row)(col) == '#' then counter += 1
      builder += counter
    Some(Key(builder.result()))

  def part1(): Int =
    val schematics = readInput()
    val locks = schematics.flatMap(parseLock)
    val keys = schematics.flatMap(parseKey)
    val height = schematics.head.length - 1
    locks.map: lock =>
      keys.count: key =>
        val res = lock.heights.zip(key.heights).forall((x, y) => x + y < height)
        res
    .sum

  def answer1: Int = 3201

  def part2(): Int = 0

  def answer2: Int = 0
