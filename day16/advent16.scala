import scala.io.Source

object Advent16 extends AdventApp {

  type Op = (Int, Int, Int, State) => State

  type BinOp[T] = (Int, Int) => T

  type Defs = Seq[Seq[(String, Op)]]

  case class Input(samples: Seq[Sample], prog: Seq[(Int, Int, Int, Int)])

  case class State(regs: Vector[Int] = Vector.fill(4)(0)) {
    def getReg(r: Int) = regs(r)
    def setReg(r: Int, n: Int) = copy(regs = regs.updated(r, n))
    override def toString: String = regs.mkString(",")
  }

  object State {
    def regs(regs: Int*) = State(regs.toVector)
  }

  case class Arg(name: String)(f: (Int, State) => Int) {
    def apply(arg: Int, st: State) = f(arg, st)
  }

  case class Sample(before: State, after: State, input: (Int, Int, Int), opCode: Int) {
    override def toString: String = {
      val (a, b, c) = input
      s"Before: [${before.regs.mkString(", ")}]\n$opCode, $a, $b, $c\nAfter: [${after.regs.mkString(", ")}]"
    }
  }

  object Op {
    def apply(f: Op): Op = f

    val argReg: Arg = Arg("r")((r, st) => st.getReg(r))
    val argImm: Arg = Arg("i")((n, _) => n)
    val argIgn: Arg = Arg("x")((_, _) => 0)

    def binOp(arg1: Arg, arg2: Arg)(op: BinOp[Int]) = Op { (a, b, c, st) =>
      st.setReg(c, op(arg1(a, st), arg2(b, st)))
    }

    def testOp(op: BinOp[Boolean]): BinOp[Int] = (a: Int, b: Int) => if(op(a, b)) 1 else 0

    def binOpArgs(name: String, args: (Arg, Arg)*)(op: BinOp[Int]) =
      for((a, b) <- args) yield (name + a.name + b.name) -> binOp(a, b)(op)

    def binRI(name: String)(op: BinOp[Int]) =
      binOpArgs(name, (argReg, argReg), (argReg, argImm))(op)

    def testIRR(name: String)(op: BinOp[Boolean]) =
      binOpArgs(name, (argImm, argReg), (argReg, argImm), (argReg, argReg))(testOp(op))

  }

  def check(sample: Sample)(op: Op) = {
    val (a, b, c) = sample.input
    op(a, b, c, sample.before) == sample.after
  }

  lazy val instructions =
    (Op.binRI("add")(_ + _) ++
      Op.binRI("mul")(_ * _) ++
      Op.binRI("ban")(_ & _) ++
      Op.binRI("bor")(_ | _) ++
      Op.binOpArgs("set", (Op.argReg, Op.argIgn), (Op.argImm, Op.argIgn))( (a, _) => a) ++
      Op.testIRR("gt")(_ > _) ++
      Op.testIRR("eq")(_ == _))

  def matches(sample: Sample) = {
    for((name, op) <- instructions if check(sample)(op)) yield name
  }

  def getInput() = {
    val source = Source.fromFile(
      args.headOption.getOrElse("day16/input16.txt")
    ).mkString
    val (samplesSrc, progSrc) = {
      val a = "\\n{4}".r.split(source)
      (a(0), a(1))
    }

    val samples = {
      def parseNums(s: String, patt: String = ", ") = patt.r.split(s).map(_.toInt).toList
      val patt = "Before:\\s+\\[(.*)\\]\n(.*)\nAfter:\\s+\\[(.*)\\]".r
      patt.findAllMatchIn(samplesSrc).map { m =>
        val args = parseNums(m.group(2), "\\s+")
        Sample(before = State(parseNums(m.group(1)).toVector),
          after = State(parseNums(m.group(3)).toVector),
          input = (args(1), args(2), args(3)),
          opCode = args(0)
        )
      }.toList
    }

    val prog = progSrc.linesIterator.map { l =>
      val ns = l.split(" ").map(_.toInt)
      (ns(0), ns(1), ns(2), ns(3))
    }.toSeq

    Input(samples, prog)
  }

  def part1(i: Input) = {
    val count =
      i.samples.filter { sample =>
        matches(sample).length >= 3
      }.length
    println(s"Part 1: $count")
  }

  def reduce(ds: Defs): Seq[Op] = {

    def remove(name: String, ds: Defs) =
      ds.map(cands => if(cands.length > 1) cands.filterNot(_._1 == name) else cands)

    val res = ds.foldLeft(ds) { (acc, d) =>
      if(d.length == 1) {
        val (name, _) = d.head
        remove(name, acc)
      } else acc
    }

    if(res.exists(_.length > 1)) reduce(res) else {
      res.map(_.head._2)
    }
  }

  def part2(i: Input) = {

    val defs = reduce(
      i.samples.foldLeft(Vector.fill(instructions.length)(instructions)) { (acc, sample) =>
        val c = sample.opCode
        val cands = acc(c)
        val filtered = cands.filter(i => check(sample)(i._2))
        acc.updated(c, filtered)
      }
    )

    val res = i.prog.foldLeft(State()) { (st, ins) =>
      val (opcode, a, b, c) = ins
      val op = defs(opcode)
      op(a, b, c, st)
    }
    println(s"Part 2: ${res.getReg(0)}")
  }

}
