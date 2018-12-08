sealed trait Event
case class LogLine(dt: String, event: Event)

case class Guard(id: Int) extends Event
case object Sleeps extends Event
case object Wakes extends Event

val linePat = """\[(.*)\] (.*)""".r
val guardPat = """Guard #([0-9]+) begins shift""".r

def parse(s: String): LogLine = s match {
  case `linePat`(dt, evRaw) =>
    val ev = evRaw match {
      case `guardPat`(id) => Guard(Integer.parseInt(id))
      case "falls asleep" => Sleeps
      case "wakes up" => Wakes
    }
    LogLine(dt, ev)
}

type Record = Map[Int, Vector[Int]]

def minute(dt: String): Int = Integer.parseInt(dt.split(":")(1))

def addSleep(start: Int, end: Int, acc: Vector[Int]) =
  acc.zipWithIndex map {
    case (x, idx) => if(idx >= start && idx < end) x + 1 else x
  }

def processShift(input: Seq[LogLine], acc: Vector[Int]): (Vector[Int], Seq[LogLine]) =
  input match {
    case LogLine(sleepStart, Sleeps) :: LogLine(sleepEnd, Wakes) :: rest =>
      processShift(rest, addSleep(minute(sleepStart), minute(sleepEnd), acc))
    case LogLine(_, _: Guard) :: _ => (acc, input)
    case Nil => (acc, input)
  }

def process(logs: Seq[LogLine], acc: Record = Map.empty): Record = logs match {
  case Nil => acc
  case LogLine(_, Guard(id)) :: rest =>
    val (nextAcc, nextInput) = processShift(rest, acc.getOrElse(id, Vector.fill(60)(0)))
    process(nextInput, acc.updated(id, nextAcc))
}

def part1(rec: Record) = {
  val (guard, sleeps) = rec.maxBy { case (k, v) =>
    v.sum
  }
  val (_, min) = sleeps.zipWithIndex.maxBy {
    case (freq, min) => freq
  }
  guard * min
}

@main
def main(fname: String = "input04.txt") = {
  val input =
    scala.io.Source.fromFile(fname)
      .getLines
      .toList
      .map(parse)
      .sortBy(_.dt)
  val data = process(input)
  println(s"Part 1: ${part1(data)}")
}
