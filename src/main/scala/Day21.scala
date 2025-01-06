import scala.collection.mutable
import scala.io.Source

object Day21:
  def readInput(): Vector[String] =
    Source.fromResource("day21.txt").getLines().toVector

  enum NumericKey(val c: Char, val x: Int, val y: Int):
    case Seven extends NumericKey('7', 0, 0)
    case Eight extends NumericKey('8', 0, 1)
    case Nine extends NumericKey('9', 0, 2)
    case Four extends NumericKey('4', 1, 0)
    case Five extends NumericKey('5', 1, 1)
    case Six extends NumericKey('6', 1, 2)
    case One extends NumericKey('1', 2, 0)
    case Two extends NumericKey('2', 2, 1)
    case Three extends NumericKey('3', 2, 2)
    case Zero extends NumericKey('0', 3, 1)
    case A extends NumericKey('A', 3, 2)

    def distance(other: NumericKey): Int =
      val NumericKey(_, ox, oy) = other
      val dx = math.abs(x - ox)
      val dy = math.abs(y - oy)
      dx + dy

  object NumericKey:
    def unapply(key: NumericKey): Some[(Char, Int, Int)] = Some((key.c, key.x, key.y))

    def fromCoordinates(x: Int, y: Int): Option[NumericKey] =
      NumericKey.values.find(nk => nk.x == x && nk.y == y)

    def fromChar(c: Char): NumericKey =
      var i = 0
      val values = NumericKey.values
      while i < values.length do
        val k = values(i)
        if k.c == c then return k
        i += 1
      throw new IllegalArgumentException(s"No numeric key mathing $c")

  enum DirectionalKey(val c: Char, val x: Int, val y: Int):
    case Up extends DirectionalKey('^', 0, 1)
    case A extends DirectionalKey('A', 0, 2)
    case Left extends DirectionalKey('<', 1, 0)
    case Down extends DirectionalKey('v', 1, 1)
    case Right extends DirectionalKey('>', 1, 2)

    def distance(other: DirectionalKey): Int =
      val DirectionalKey(_, ox, oy) = other
      val dx = math.abs(x - ox)
      val dy = math.abs(y - oy)
      dx + dy

  object DirectionalKey:
    def unapply(key: DirectionalKey): Some[(Char, Int, Int)] = Some((key.c, key.x, key.y))

    def fromCoordinates(x: Int, y: Int): Option[DirectionalKey] =
      DirectionalKey.values.find(dk => dk.x == x && dk.y == y)

    def fromChar(c: Char): DirectionalKey =
      var i = 0
      val values = DirectionalKey.values
      while i < values.length do
        val k = values(i)
        if k.c == c then return k
        i += 1
      throw new IllegalArgumentException(s"No numeric key mathing $c")

  enum KeyPress(val c: Char):
    case Up extends KeyPress('^')
    case Down extends KeyPress('v')
    case Left extends KeyPress('<')
    case Right extends KeyPress('>')
    case A extends KeyPress('A')

    override def toString: String = c.toString

  private def allNumericPaths(current: NumericKey, target: NumericKey): Vector[String] =
    val builder = Vector.newBuilder[String]
    case class State(curr: NumericKey, remaining: Int, path: Vector[KeyPress])
    val queue = mutable.Queue(State(current, remaining = current.distance(target), Vector.empty))
    while queue.nonEmpty do
      val State(curr, remaining, path) = queue.dequeue()
      if remaining == 0 then
        if curr == target then
          builder += path.mkString
      else
        val NumericKey(_, x, y) = curr
        NumericKey.fromCoordinates(x - 1, y).foreach: nk =>
          val up = State(nk, remaining - 1, path :+ KeyPress.Up)
          queue += up
        NumericKey.fromCoordinates(x + 1, y).foreach: nk =>
          val down = State(nk, remaining - 1, path :+ KeyPress.Down)
          queue += down
        NumericKey.fromCoordinates(x, y - 1).foreach: nk =>
          val left = State(nk, remaining - 1, path :+ KeyPress.Left)
          queue += left
        NumericKey.fromCoordinates(x, y + 1).foreach: nk =>
          val right = State(nk, remaining - 1, path :+ KeyPress.Right)
          queue += right
    builder.result()

  private val memo: Array[Array[Array[Long]]] =
    val res = Array.ofDim[Long](DirectionalKey.values.length, NumericKey.values.length, 25)
    for
      x <- DirectionalKey.values.indices
      y <- DirectionalKey.values.indices
      z <- (0 until 25)
    do
      res(x)(y)(z) = -1
    res

  private def directionalPathComplicated(current: DirectionalKey, target: DirectionalKey, levels: Int): Long =
    if memo(current.ordinal)(target.ordinal)(levels - 1) != -1 then return memo(current.ordinal)(target.ordinal)(levels - 1)
    val paths = directionalPath(current, target)
    val res =
      if levels == 1 then
        paths.minBy(_.length).length.toLong
      else
        paths.map: path =>
          path.foldLeft((DirectionalKey.A: DirectionalKey, 0L)):
            case ((directional, acc), c) =>
            val target = DirectionalKey.fromChar(c)
            (target, acc + directionalPathComplicated(directional, target, levels - 1))
          ._2
        .min
    memo(current.ordinal)(target.ordinal)(levels - 1) = res
    res

  private def directionalPath(current: DirectionalKey, target: DirectionalKey): Vector[String] =
    if current == target then return Vector("A")
    val builder = Vector.newBuilder[String]
    case class State(curr: DirectionalKey, remaining: Int, path: Vector[KeyPress])
    val queue = mutable.Queue(State(current, remaining = current.distance(target), Vector.empty))
    while queue.nonEmpty do
      val State(curr, remaining, path) = queue.dequeue()
      if remaining == 0 then
        if curr == target then
          builder += (path.mkString + "A")
      else
        val DirectionalKey(_, x, y) = curr
        DirectionalKey.fromCoordinates(x - 1, y).foreach: dk =>
          val up = State(dk, remaining - 1, path :+ KeyPress.Up)
          queue += up
        DirectionalKey.fromCoordinates(x + 1, y).foreach: dk =>
          val down = State(dk, remaining - 1, path :+ KeyPress.Down)
          queue += down
        DirectionalKey.fromCoordinates(x, y - 1).foreach: dk =>
          val left = State(dk, remaining - 1, path :+ KeyPress.Left)
          queue += left
        DirectionalKey.fromCoordinates(x, y + 1).foreach: dk =>
          val right = State(dk, remaining - 1, path :+ KeyPress.Right)
          queue += right
    builder.result()

  private def numericPart(code: String): Long =
    code.take(3).toLong

  private def raise(one: String, levels: Int): Long =
    one.foldLeft((DirectionalKey.A: DirectionalKey, 0L)):
      case ((directional, acc), c) =>
        val target = DirectionalKey.fromChar(c)
        (target, acc + directionalPathComplicated(directional, target, levels))
    ._2

  private def allCombinations(rows: Vector[Vector[String]]): Vector[String] =
    var buffer = mutable.ArrayBuffer("")
    var r = 0
    while r < rows.length do
      val row = rows(r)
      val newBuffer = mutable.ArrayBuffer.empty[String]
      row.foreach: c =>
        val tmpBuffer = buffer.to(mutable.ArrayBuffer)
        tmpBuffer.mapInPlace(_ + c + "A")
        newBuffer ++= tmpBuffer
      buffer = newBuffer
      r += 1
    buffer.toVector

  private def solve(depth: Int): Long =
    val codes = readInput()
    codes.map: code =>
      val numericPaths = code.foldLeft((NumericKey.A: NumericKey, Vector.empty[Vector[String]])):
        case ((numeric, acc), c) =>
          val target = NumericKey.fromChar(c)
          (target, acc :+ allNumericPaths(numeric, target))
      ._2
      val all = allCombinations(numericPaths)
      val raised = all.map(raise(_, depth))
      val l = raised.min
      val n = numericPart(code)
      l * n
    .sum

  def part1(): Long = solve(2)

  def answer1: Long = 163920L

  def part2(): Long = solve(25)

  def answer2: Long = 204040805018350L
