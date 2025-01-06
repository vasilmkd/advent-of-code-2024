import scala.collection.mutable
import scala.io.Source

object Day12:
  type Grid = Array[Array[Char]]

  def readInput(): Grid =
    Source.fromResource("day12.txt").getLines().map(_.toCharArray).toArray

  case class Plot(x: Int, y: Int):
    def up: Plot = Plot(x - 1, y)
    def down: Plot = Plot(x + 1, y)
    def left: Plot = Plot(x, y - 1)
    def right: Plot = Plot(x, y + 1)

    def inBounds(grid: Grid): Boolean =
      x >= 0 && x < grid.length && y >= 0 && y < grid(x).length

    def fences(grid: Grid, region: Region): Int =
      4 - Set(up, down, left, right).filter(_.inBounds(grid)).count(region.plots.contains)

    def upSegment: Segment = Segment(x, y, x, y + 1, Orientation.Horizontal)
    def downSegment: Segment = Segment(x + 1, y, x + 1, y + 1, Orientation.Horizontal)
    def leftSegment: Segment = Segment(x, y, x + 1, y, Orientation.Vertical)
    def rightSegment: Segment = Segment(x, y + 1, x + 1, y + 1, Orientation.Vertical)

    def segments(grid: Grid, region: Region): Set[Segment] =
      val res = mutable.Set.empty[Segment]
      if !up.inBounds(grid) || !region.plots.contains(up) then res += upSegment
      if !down.inBounds(grid) || !region.plots.contains(down) then res += downSegment
      if !left.inBounds(grid) || !region.plots.contains(left) then res += leftSegment
      if !right.inBounds(grid) || !region.plots.contains(right) then res += rightSegment
      res.toSet

  private def bfs(grid: Grid, plot: Plot): Region =
    val plant = grid(plot.x)(plot.y)
    val queue = mutable.Queue(plot)
    val seen = mutable.Set.empty[Plot]
    while queue.nonEmpty do
      val p = queue.dequeue()
      if !seen.contains(p) then
        seen += p
        val up = p.up
        if up.inBounds(grid) && grid(up.x)(up.y) == plant then queue.enqueue(up)
        val down = p.down
        if down.inBounds(grid) && grid(down.x)(down.y) == plant then queue.enqueue(down)
        val left = p.left
        if left.inBounds(grid) && grid(left.x)(left.y) == plant then queue.enqueue(left)
        val right = p.right
        if right.inBounds(grid) && grid(right.x)(right.y) == plant then queue.enqueue(right)
    Region(seen.toSet, plant)

  case class Region(plots: Set[Plot], plant: Char):
    def area: Int = plots.size

    def perimeter(grid: Grid): Int =
      plots.toVector.map(_.fences(grid, this)).sum

    def sides(grid: Grid): Int =
      def next(segments: Set[Segment]): Set[Segment] =
        val res = segments.to(mutable.Set)
        val it1 = segments.iterator
        while it1.hasNext do
          val a = it1.next()
          val it2 = segments.iterator
          while it2.hasNext do
            val b = it2.next()
            a.connect(b, segments) match
              case Some(c) =>
                res -= a
                res -= b
                res += c
                return res.toSet
              case None =>
        res.toSet

      var segs = plots.flatMap(_.segments(grid, this))
      var cont = true
      while cont do
        val n = next(segs)
        if segs != n then
          segs = n
        else
          cont = false
      segs.size

  private def regions(grid: Grid): Vector[Region] =
    val seen = mutable.Set.empty[Plot]
    val res = mutable.Set.empty[Region]
    var i = 0
    while i < grid.length do
      var j = 0
      while j < grid(i).length do
        val p = Plot(i, j)
        if !seen.contains(p) then
          val region = bfs(grid, p)
          seen ++= region.plots
          res += region
        j += 1
      i += 1
    res.toVector

  enum Orientation:
    case Horizontal
    case Vertical

  case class Segment(sx: Int, sy: Int, ex: Int, ey: Int, orientation: Orientation):
    def connect(other: Segment, segments: Set[Segment]): Option[Segment] =
      val Segment(asx, asy, aex, aey, ao) = summon[Ordering[Segment]].min(this, other)
      val Segment(bsx, bsy, bex, bey, bo) = summon[Ordering[Segment]].max(this, other)
      if ao != bo then return None
      if aex == bsx && aey == bsy then
        val spoil = segments.exists:
          case Segment(csx, csy, _, _, co) =>
            csx == bsx && csy == bsy && bo != co
        if spoil then return None
        return Some(Segment(asx, asy, bex, bey, ao))
      None

  object Segment:
    given Ordering[Segment] =
      (x: Segment, y: Segment) =>
        if x.sx != y.sx then x.sx - y.sx
        else if x.sy != y.sy then x.sy - y.sy
        else if x.ex != y.ex then x.ex - y.ex
        else x.ey - y.ey

  def part1(): Int =
    val g = readInput()
    val rs = regions(g)
    rs.map(r => r.area * r.perimeter(g)).sum

  def answer1: Int = 1451030

  def part2(): Int =
    val g = readInput()
    val rs = regions(g).sortBy(_.plant)
    rs.map(r => r.area * r.sides(g)).sum

  def answer2: Int = 859494
