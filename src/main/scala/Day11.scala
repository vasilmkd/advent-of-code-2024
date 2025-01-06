import scala.collection.mutable
import scala.io.Source

object Day11:
  def readInput(): Array[Long] =
    Source.fromResource("day11.txt").getLines().map(_.split(' ').map(_.toLong)).toArray.flatten

  private def simulate(numbers: Array[Long], depth: Int): Long =
    val current = new Array[mutable.Map[Long, Long]](depth + 1)
    (0 to depth).foreach(i => current(i) = mutable.Map.empty)
    numbers.foreach: n =>
      current(depth) += (n -> 1L)
    var result = 0L
    var currentDepth = depth
    while currentDepth >= 0 do
      val (n, c) = current(currentDepth).head
      current(currentDepth) -= n
      if currentDepth == 0 then
        result += c
      else
        if n == 0L then
          val old = current(currentDepth - 1).getOrElse(1L, 0L)
          current(currentDepth - 1) += (1L -> (old + c))
        else
          val s = n.toString
          val len = s.length
          if len % 2 == 0 then
            val length =
              var x = n
              var cnt = 0
              while x > 0 do
                x /= 10
                cnt += 1
              cnt

            val half = length / 2
            // Calculate divisor using exp instead of loop
            val divisor = math.pow(10, half).toLong
            val l = n % divisor
            val r = n / divisor
            val old1 = current(currentDepth - 1).getOrElse(l, 0L)
            current(currentDepth - 1) += (l -> (old1 + c))
            val old2 = current(currentDepth - 1).getOrElse(r, 0L)
            current(currentDepth - 1) += (r -> (old2 + c))
          else
            val old = current(currentDepth - 1).getOrElse(n * 2024, 0L)
            current(currentDepth - 1) += (n * 2024 -> (old + c))
      if current(currentDepth).isEmpty then
        currentDepth -= 1
    result

  def part1(): Long =
    val input = readInput()
    simulate(input, 25)

  def answer1: Long = 202019L

  def part2(): Long =
    val input = readInput()
    simulate(input, 75)

  def answer2: Long = 239321955280205L
