@annotation.tailrec
def reduce1(s: List[Char], acc: List[Char] = Nil): List[Char] = s match {
  case c1 :: c2 :: rest =>
    if(c1.toUpper == c2.toUpper && c1 != c2)
      reduce1(rest, acc)
    else
      reduce1(c2 :: rest, c1 :: acc)
  case c1 :: Nil => c1 :: acc
  case Nil => acc
}

@annotation.tailrec
def reduce(s: List[Char]): List[Char] = {
  val next = reduce1(s)
  if(next.length == s.length) next else reduce(next)
}

def part2(s: List[Char]) = {
  val uniq = s.map(_.toUpper).toSet.toList
  uniq.map { p =>
    val without = s.filterNot(_.toUpper == p)
    reduce(without).length
  }.min
}

@main
def main(fname: String = "input05.txt") = {
  val input = scala.io.Source
    .fromFile(fname)
    .mkString
    .trim
    .toList
  println(s"Part 1: ${reduce(input).length}")
  println(s"Part 2: ${part2(input)}")
}
