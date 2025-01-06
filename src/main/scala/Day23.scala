import scala.collection.mutable
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.io.Source

object Day23:
  case class ComputerName(name: String):
    def interesting: Boolean = name.charAt(0) == 't'

  object ComputerName:
    given Ordering[ComputerName] = Ordering.by(_.name)

  case class Connection(first: ComputerName, second: ComputerName)

  def readInput(): LazyList[Connection] =
    Source
      .fromResource("day23.txt")
      .getLines()
      .map(_.split('-').map(ComputerName.apply))
      .collect:
        case Array(first, second) => Connection(first, second)
      .to(LazyList)

  private def adjacency(input: LazyList[Connection]): Map[ComputerName, Set[ComputerName]] =
    val res = mutable.Map.empty[ComputerName, mutable.Set[ComputerName]]
    input.foreach:
      case Connection(first, second) =>
        res.getOrElseUpdate(first, mutable.Set.empty) += second
        res.getOrElseUpdate(second, mutable.Set.empty) += first
    res.map((cn, set) => (cn, set.toSet)).toMap

  private def triples(matrix: Map[ComputerName, Set[ComputerName]]): Set[Set[ComputerName]] =
    val res = mutable.Set.empty[Set[ComputerName]]
    matrix.keysIterator.foreach: cn1 =>
      matrix(cn1).foreach: cn2 =>
        matrix(cn2).foreach: cn3 =>
          if matrix(cn3).contains(cn1) then
            res += Set(cn1, cn2, cn3)
    res.toSet

  private def grow(graph: Map[ComputerName, Set[ComputerName]], triple: Set[ComputerName]): Set[ComputerName] =
    val clique = triple.to(mutable.Set)

    def belongs(graph: Map[ComputerName, Set[ComputerName]], newComputer: ComputerName): Boolean =
      !clique.contains(newComputer) && clique.forall(c => graph(c).contains(newComputer))

    var cont = true
    while cont do
      val oldSize = clique.size
      graph.keysIterator.find(belongs(graph, _)).foreach(clique += _)
      val newSize = clique.size
      cont = newSize > oldSize
    clique.toSet

  def part1(): Int =
    val input = readInput()
    val matrix = adjacency(input)
    val ts = triples(matrix)
    ts.count(_.exists(_.interesting))

  def answer1: Int = 1327

  extension [A](as: Set[A]) def parMap[B](f: A => B): Set[B] =
    import scala.concurrent.ExecutionContext.Implicits.global
    val futures = as.map(a => Future(f(a)))
    Await.result(Future.sequence(futures), Duration.Inf)

  def part2(): String =
    val input = readInput()
    val matrix = adjacency(input)
    val ts = triples(matrix)
    val maxCliques = ts.parMap(grow(matrix, _))
    val largest = maxCliques.maxBy(_.size)
    largest.toVector.sorted.map(_.name).mkString(",")

  def answer2: String = "df,kg,la,mp,pb,qh,sk,th,vn,ww,xp,yp,zk"
