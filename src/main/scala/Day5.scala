import scala.collection.mutable
import scala.io.Source

object Day5:
  extension [A](as: IArray[A]) private def arrayString: String = as.mkString("IArray(", ", ", ")")
  extension [A](as: IArray[IArray[A]]) private def nestedArrayString: String =
    as.foldLeft("IArray(")((str, as) => str ++ as.arrayString ++ ", ").stripSuffix(", ") ++ ")"

  case class Input(edges: IArray[(Int, Int)], updates: IArray[IArray[Int]]):
    override def toString: String = s"Input(${edges.arrayString}, ${updates.nestedArrayString})"

  def readInput(): Input =
    Source.fromResource("day5.txt").getLines().foldLeft(Input(IArray.empty, IArray.empty)):
      case (input, "") => input
      case (Input(edges, updates), line) if line.contains("|") =>
        val Array(first, second) = line.split("""\|""").map(_.toInt)
        Input(edges :+ (first, second), updates)
      case (Input(edges, updates), line) if line.contains(",") =>
        val numbers = line.split(",").map(_.toInt)
        val iarray = IArray.unsafeFromArray(numbers)
        Input(edges, IArray.unsafeFromArray(IArray.genericWrapArray(updates).appended(iarray).toArray))

  case class Edge(from: Int, to: Int)

  case class Graph(nodes: Set[Int], edges: Set[Edge]):
    override def toString: String = s"Graph($nodes, $edges)"

    def isTopologicallySorted(update: IArray[Int]): Boolean =
      val map = update.zipWithIndex.toMap
      edges.forall:
        case Edge(from, to) =>
          val x = map.getOrElse(from, -1)
          val y = map.getOrElse(to, -1)
          x == -1 || y == -1 || x < y

  private def kahns(toSort: Set[Int], edges: Set[Edge]): IArray[Int] =
    def noIncoming(n: Int, e: mutable.Set[Edge]): Boolean =
      e.forall:
        case Edge(from, to) => to != n || !toSort.contains(from)

    val builder = IArray.newBuilder[Int]
    val mutableEdges = edges.to(mutable.Set)
    val s = toSort.filter(noIncoming(_, mutableEdges)).to(mutable.Set)

    while s.nonEmpty do
      val n = s.head
      s.remove(n)
      builder += n
      val candidates = mutableEdges.filter:
        case Edge(`n`, _) => true
        case _ => false
      candidates.foreach:
        case e @ Edge(_, m) =>
          mutableEdges.remove(e)
          if toSort.contains(m) && noIncoming(m, mutableEdges) then
            s += m
    end while
    builder.result()

  private object Graph:
    def fromInput(input: Input): Graph =
      val nodes = input.edges.flatMap((x, y) => IArray(x, y)).toSet
      val edges = input.edges.map((x, y) => Edge(x, y)).toSet
      Graph(nodes, edges)

  private def middle(numbers: IArray[Int]): Int =
    val index = numbers.length / 2
    numbers(index)

  def part1(): Int =
    val input = readInput()
    val graph = Graph.fromInput(input)
    input.updates.filter(graph.isTopologicallySorted).map(middle).sum

  def answer1: Int = 5713

  def part2(): Int =
    val input = readInput()
    val graph = Graph.fromInput(input)
    input.updates
      .filterNot(graph.isTopologicallySorted)
      .map(update => kahns(update.toSet, graph.edges))
      .map(middle)
      .sum

  def answer2: Int = 5180
