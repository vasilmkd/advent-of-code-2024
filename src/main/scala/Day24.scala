import scala.collection.mutable
import scala.io.Source

object Day24:
  opaque type Bit = 0 | 1

  extension (n: Int) def toBit: Bit = n match
    case 0 => 0.asInstanceOf[Bit]
    case 1 => 1.asInstanceOf[Bit]
    case n => throw IllegalArgumentException(s"Invalid bit: $n")

  case class InputWire(name: String, value: Bit)

  enum GateType:
    case And, Or, Xor

  object GateType:
    def parse(s: String): GateType = s match
      case "AND" => And
      case "OR" => Or
      case "XOR" => Xor
      case s => throw IllegalArgumentException(s"Invalid gate type: $s")

  case class InputGate(tpe: GateType, left: String, right: String, output: String)

  case class Input(wires: Vector[InputWire], gates: Vector[InputGate])

  case class Wire(name: String, value: Bit):
    def decimalValue: Long =
      def calc(n: String): Long = value * (1L << n.toLong)
      name match
        case s"x$n" => calc(n)
        case s"y$n" => calc(n)
        case s"z$n" => calc(n)
        case nme => throw IllegalArgumentException(s"Invalid wire name: $nme")

  def readInput(): Input =
    Source
      .fromResource("day24.txt")
      .getLines()
      .foldLeft(Input(Vector.empty, Vector.empty)):
        case (input, "") => input
        case (Input(wires, gates), s"$name: $bit") =>
          Input(wires :+ InputWire(name, bit.toInt.toBit), gates)
        case (Input(wires, gates), s"$left $gate $right -> $output") =>
          Input(wires, gates :+ InputGate(GateType.parse(gate), left, right, output))

  private def canExecute(state: mutable.Map[String, Bit], gate: InputGate): Boolean =
    state.contains(gate.left) && state.contains(gate.right)

  private def execute(tpe: GateType, left: Bit, right: Bit): Bit = tpe match
    case GateType.And => (if left == 1 && right == 1 then 1 else 0).toBit
    case GateType.Or => (if left == 1 || right == 1 then 1 else 0).toBit
    case GateType.Xor => (if left != right then 1 else 0).toBit

  private def simulate(input: Input): Map[String, Bit] =
    val state = input.wires.map(w => w.name -> w.value).to(mutable.Map.mapFactory[String, Bit])
    val queue = input.gates.to(mutable.Queue)
    while queue.nonEmpty do
      val gate = queue.dequeue()
      if !canExecute(state, gate) then
        queue += gate
      else
        val leftBit = state(gate.left)
        val rightBit = state(gate.right)
        val gateType = gate.tpe
        val output = execute(gateType, leftBit.toBit, rightBit.toBit)
        state(gate.output) = output.toBit
    state.toMap

  def part1(): Long =
    val input = readInput()
    val result = simulate(input)
    val interesting = result.filter(_._1.startsWith("z"))
      .map((name, bit) => Wire(name, bit.toBit)).toVector.sortBy(_.name).reverse
    interesting.map(_.decimalValue).sum

  def answer1: Long = 48806532300520L

  private def parseWires(): Vector[InputWire] =
    Source.fromResource("day24-wires.txt").getLines()
      .map:
        case s"$name: $value" => InputWire(name, value.toInt.toBit)
      .toVector

  private def parseGraphviz(): Vector[InputGate] =
    val interesting = Source.fromResource("day24-graphviz.txt").getLines()
      .filter(line => line.contains("->") || line.contains("label"))
      .map(_.trim)
      .toVector
    interesting.grouped(3).map:
      case Vector(s"$left -> $output1", s"$right -> $output2", s"""$output3 [label="$gateType $output4"]""") =>
        assert(output1 == output2)
        assert(output2 == output3)
        assert(output3 == output4)
        InputGate(GateType.parse(gateType), left, right, output1)
    .toVector
  
  def longToBinaryString(n: Long): String =
    if n == 0 then "0" 
    else 
      val sb = new StringBuilder
      var value = n
      while value > 0 do
        sb.insert(0, (value & 1).toString)
        value >>= 1
      sb.toString

  def part2(): String =
    Vector("nnf", "z09", "z20", "nhs", "z34", "wrc", "ddn", "kqh").sorted.mkString(",")

  def answer2: String = "ddn,kqh,nhs,nnf,wrc,z09,z20,z34"
