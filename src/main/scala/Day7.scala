import java.util.concurrent.CountDownLatch
import java.util.concurrent.atomic.LongAdder
import scala.collection.immutable.ArraySeq
import scala.collection.mutable
import scala.concurrent.Future
import scala.io.Source

object Day7:
  case class Equation(target: Long, operands: IArray[Long]):
    override def toString: String = s"Equation($target, ${operands.mkString("Array(", ", ", ")")})"

  private object Equation:
    def parse(line: String): Equation = line match
      case s"$target: $operands" =>
        Equation(target.toLong, IArray.unsafeFromArray(operands.split(' ').map(_.toLong)))

  def readInput(): ArraySeq[Equation] =
    Source.fromResource("day7.txt").getLines().map(Equation.parse).to(ArraySeq)

  private case class State(target: Long, current: Long, index: Int, operands: IArray[Long])

  private enum Op(val f: (Long, Long) => Long):
    case Add extends Op(_ + _)
    case Multiply extends Op(_ * _)
    case Concat extends Op(concat)

  private def bfs(ops: IArray[Op])(equation: Equation): Boolean =
    val Equation(target, operands) = equation
    val start = State(target, operands.head, 1, operands)
    val queue = mutable.Queue(start)
    while queue.nonEmpty do
      val State(t, c, i, o) = queue.dequeue()
      if c == t && i == o.length then
        return true
      if i < o.length then
        ops.foreach: op =>
          queue.enqueue(State(t, op.f(c, o(i)), i + 1, o))
    false

  private def parCount(equations: ArraySeq[Equation])(search: Equation => Boolean): Long =
    import scala.concurrent.ExecutionContext.Implicits.global
    val adder = LongAdder()
    val latch = CountDownLatch(equations.size)
    equations.foreach: e =>
      Future:
        if search(e) then
          adder.add(e.target)
        latch.countDown()
    latch.await()
    adder.longValue()

  private inline def concat(a: Long, b: Long): Long =
    a.toString.concat(b.toString).toLong

  def part1(): Long =
    val input = readInput()
    parCount(input)(bfs(IArray(Op.Add, Op.Multiply)))

  def answer1: Long = 4555081946288L

  def part2(): Long =
    val input = readInput()
    parCount(input)(bfs(IArray(Op.Add, Op.Multiply, Op.Concat)))

  def answer2: Long = 227921760109726L
