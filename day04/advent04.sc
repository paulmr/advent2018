sealed trait Event
case class LogLine(dt: String, event: String)
type Record = Map[Int, Vector[Int]]

val linePat = """\[(.*)\] (.*)""".r
val guardPat = """Guard #([0-9]+) begins shift""".r

def parse(s: String): LogLine = s match { case `linePat`(dt, ev) => LogLine(dt, ev) }

def minute(dt: String): Int = Integer.parseInt(dt.split(":")(1))

def addSleep(start: Int, end: Int, acc: Vector[Int]) =
  acc.zipWithIndex map { case (x, idx) => if(idx >= start && idx < end) x + 1 else x }

def processShift(input: Seq[LogLine], acc: Vector[Int]): (Vector[Int], Seq[LogLine]) =
  input match {
    case LogLine(sleepStart, "falls asleep") :: LogLine(sleepEnd, "wakes up") :: rest =>
      processShift(rest, addSleep(minute(sleepStart), minute(sleepEnd), acc))
    case _ => (acc, input)
  }

def process(logs: Seq[LogLine], acc: Record = Map.empty): Record = logs match {
  case Nil => acc
  case LogLine(_, `guardPat`(idStr)) :: rest =>
    val id = Integer.parseInt(idStr)
    val (nextAcc, nextInput) = processShift(rest, acc.getOrElse(id, Vector.fill(60)(0)))
    process(nextInput, acc.updated(id, nextAcc))
}

def part1(rec: Record) = {
  val (guard, sleeps) = rec.maxBy { case (k, v) => v.sum }
  val (_, min) = sleeps.zipWithIndex.maxBy { case (freq, min) => freq }
  guard * min
}

def part2(rec: Record) = {
  val (guard, (_, min)) = rec.mapValues { sleeps =>
    sleeps.zipWithIndex.maxBy { case (days, min) => days }
  }.maxBy { case (guard, (days, min)) => days }
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
  println(s"Part 2: ${part2(data)}")
}
