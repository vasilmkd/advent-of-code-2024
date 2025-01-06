import scala.io.Source
import scala.util.boundary
import scala.util.boundary.break

object Day19:
  private def readInput(): (Vector[String], Vector[String]) =
    val lines = Source.fromResource("day19.txt").getLines().toVector
    val dictionary = lines.head.split(",\\s+").toVector
    val targets = lines.drop(2)
    (dictionary, targets)

  private enum Stripe(val char: Char, val hash: Int):
    case W extends Stripe('w', 0)
    case U extends Stripe('u', 1)
    case B extends Stripe('b', 2)
    case R extends Stripe('r', 3)
    case G extends Stripe('g', 4)

  private object Stripe:
    def fromChar(c: Char): Stripe = c match
      case 'w' => W
      case 'u' => U
      case 'b' => B
      case 'r' => R
      case 'g' => G
      case _ => throw IllegalArgumentException(s"Invalid char: $c")

  private class TrieNode:
    var endOfWord: Boolean = false
    val children: Array[TrieNode] = new Array(Stripe.values.length)

  private class Trie:
    private val root: TrieNode = TrieNode()

    def insert(s: String): Unit =
      var prev = root
      s.foreach: c =>
        val stripe = Stripe.fromChar(c)
        if prev.children(stripe.hash) eq null then
          prev.children(stripe.hash) = TrieNode()
        prev = prev.children(stripe.hash)
      prev.endOfWord = true

    def countWays(s: String): Long =
      val n = s.length
      val count = new Array[Long](n)
      var i = 0
      while i < n do
        var ptr = root
        boundary[Unit]:
          var j = i
          while j >= 0 do
            val c = s.charAt(j)
            val stripe = Stripe.fromChar(c)
            if ptr.children(stripe.hash) eq null then
              break(())
            ptr = ptr.children(stripe.hash)
            if ptr.endOfWord then
              val r = if j > 0 then count(j - 1) else 1L
              count(i) += r
            j -= 1
        i += 1
      count(n - 1)
  end Trie

  private object Trie:
    def fromDictionary(dictionary: Vector[String]): Trie =
      val trie = Trie()
      dictionary.foreach: word =>
        trie.insert(word.reverse)
      trie

  def part1(): Long =
    val (dictionary, targets) = readInput()
    val trie = Trie.fromDictionary(dictionary)
    targets.count(target => trie.countWays(target) > 0)

  def answer1: Long = 369L

  def part2(): Long =
    val (dictionary, targets) = readInput()
    val trie = Trie.fromDictionary(dictionary)
    targets.map(target => trie.countWays(target)).sum

  def answer2: Long = 761826581538190L
