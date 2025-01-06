import scala.annotation.tailrec
import scala.io.Source

object Day17:
  type Int3 = 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7

  extension (n: Int) def toInt3: Int3 = n match
    case x: Int3 => x
    case x => throw IllegalArgumentException(s"Invalid value: $x")

  case class Input(ra: Long, rb: Long, rc: Long, program: Vector[Int3])

  def readInput(): Input =
    val Vector(la, lb, lc, _, lp) = Source.fromResource("day17.txt").getLines().toVector
    val ra = la match
      case s"Register A: $a" => a.toLong
    val rb = lb match
      case s"Register B: $b" => b.toLong
    val rc = lc match
      case s"Register C: $c" => c.toLong
    val program = lp match
      case s"Program: $p" => p.split(',').map(_.toInt.toInt3)
    Input(ra, rb, rc, program.toVector.asInstanceOf[Vector[Int3]])

  case class Computer(ra: Long, rb: Long, rc: Long, pc: Int, output: Vector[Int3])

  enum Operand(val raw: Int3):
    case Literal(override val raw: Int3) extends Operand(raw)
    case Combo(override val raw: Int3) extends Operand(raw)

    def value(computer: Computer): Long = this match
      case Literal(n) => n.toLong
      case Combo(4) => computer.ra
      case Combo(5) => computer.rb
      case Combo(6) => computer.rc
      case Combo(n) => n.toLong

  // Report unnecessary .asInstanceOf
  enum Instruction(val opcode: Int3, val o: Operand):
    case Adv(override val o: Operand.Combo) extends Instruction(0, o)
    case Bxl(override val o: Operand.Literal) extends Instruction(1, o)
    case Bst(override val o: Operand.Combo) extends Instruction(2, o)
    case Jnz(override val o: Operand.Literal) extends Instruction(3, o)
    case Bxc(override val o: Operand.Literal) extends Instruction(4, o)
    case Out(override val o: Operand.Combo) extends Instruction(5, o)
    case Bdv(override val o: Operand.Combo) extends Instruction(6, o)
    case Cdv(override val o: Operand.Combo) extends Instruction(7, o)

    def eval(computer: Computer): Computer = this match
      case Adv(o) =>
        val num = computer.ra
        val den = pow2(o.value(computer))
        val res = num / den
        computer.copy(ra = res, pc = computer.pc + 2)
      case Bxl(o) =>
        val b = computer.rb
        val x = o.value(computer)
        val res = b ^ x
        computer.copy(rb = res, pc = computer.pc + 2)
      case Bst(o) =>
        val x = o.value(computer)
        val res = x % 8
        computer.copy(rb = res, pc = computer.pc + 2)
      case Jnz(o) =>
        computer.ra match
          case 0L => computer.copy(pc = computer.pc + 2)
          case _ =>
            val t = o.value(computer).toInt
            computer.copy(pc = t)
      case Bxc(_) =>
        val b = computer.rb
        val c = computer.rc
        val res = b ^ c
        computer.copy(rb = res, pc = computer.pc + 2)
      case Out(o) =>
        val res = o.value(computer) % 8
        computer.copy(pc = computer.pc + 2, output = computer.output :+ res.toInt.toInt3)
      case Bdv(o) =>
        val num = computer.ra
        val den = pow2(o.value(computer))
        val res = num / den
        computer.copy(rb = res, pc = computer.pc + 2)
      case Cdv(o) =>
        val num = computer.ra
        val den = pow2(o.value(computer))
        val res = num / den
        computer.copy(rc = res, pc = computer.pc + 2)

  private inline def pow2(n: Long): Long = 1L << n

  private def decode(opcode: Int3, operand: Int3): Instruction = opcode match
    case 0 => Instruction.Adv(Operand.Combo(operand))
    case 1 => Instruction.Bxl(Operand.Literal(operand))
    case 2 => Instruction.Bst(Operand.Combo(operand))
    case 3 => Instruction.Jnz(Operand.Literal(operand))
    case 4 => Instruction.Bxc(Operand.Literal(operand))
    case 5 => Instruction.Out(Operand.Combo(operand))
    case 6 => Instruction.Bdv(Operand.Combo(operand))
    case 7 => Instruction.Cdv(Operand.Combo(operand))

  @tailrec
  private def foldLeft(program: Vector[Int3])(computer: Computer)(f: (Computer, Instruction) => Computer): Computer =
    val pc = computer.pc
    if pc >= program.length then return computer
    val opcode = program.apply(pc)
    val operand = program.apply(pc + 1)
    val instruction = decode(opcode.toInt3, operand.toInt3)
    val next = f(computer, instruction)
    foldLeft(program)(next)(f)

  def part1(): String =
    val Input(ra, rb, rc, program) = readInput()
    val s = Computer(ra, rb, rc, 0, Vector.empty)
    val c = foldLeft(program)(s)((computer, instruction) => instruction.eval(computer))
    c.output.mkString(",")

  def answer1: String = "2,3,6,2,1,6,1,2,1"

  def part2(): Long =
    val Input(_, rb, rc, program) = readInput()
    var len = 1
    var ra = 0L
    while len <= program.length do
      val target = program.takeRight(len)
      var cont = true
      while cont do
        val s = Computer(ra, rb, rc, 0, Vector.empty)
        val c = foldLeft(program)(s)((c, i) => i.eval(c))
        if c.output == target then
          ra *= 8
          len += 1
          cont = false
        else
          ra += 1
    ra / 8

  def answer2: Long = 90938893795561L
