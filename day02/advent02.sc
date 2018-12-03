import Math.min

def freq[T](ts:Seq[T]) =
  ts.foldLeft(
    Map.empty[T, Int].withDefault(_ => 0)
  ) { (acc, c) =>
    acc.updated(c, acc(c) + 1)
  }

def checksum1(s:String) =
  freq(freq(s).values.toList)
    .mapValues(n => min(1, n))

def checksum(ss: List[String]) =
  ss.foldLeft(Map.empty[Int, Int]) { (acc0, s) =>
    checksum1(s).foldLeft(acc0) { (acc1, kv) =>
      val (k, v) = kv
      acc1.updated(k, v + acc1.get(k).getOrElse(0))
    }
}

def diff(s1:String, s2:String) =
  (s1 zip s2) filter {
    case (a, b) => a != b
  }

def search(ss:List[String]):(String, String) =
  ss.tail
    .find(s => diff(ss.head, s).length == 1)
    .map(ss.head -> _)
    .getOrElse(search(ss.tail))

def part1(ss:List[String]) =
{
  val checks = checksum(ss)
  checks(3) * checks(2)
}

def part2(ids:List[String]) =
{
  val (a, b) = search(ids)
  a.zip(b).collect {
    case (a,b) if a==b => a
  }.mkString
}

@main
def main() =
{
  val input = scala.io.Source
    .fromFile("input02.txt")
    .getLines
    .toList

  println(s"Part 1: ${part1(input)}")
  println(s"Part 2: ${part2(input)}")
}
