def calc(e1: Int = 0, e2: Int = 1, init: Vector[Int] = Vector(3, 7)): Stream[Int] = {
  def go(e1: Int, e2: Int, history: Vector[Int]): Stream[Int] = {
    val sum = history(e1) + history(e2)
    val newNums =
      if(sum > 9) Vector(sum / 10, sum % 10) else Vector(sum % 10)
    val newHist = history ++ newNums
    newNums.toStream #::: go(
      (newHist(e1) + 1 + e1) % newHist.length,
      (newHist(e2) + 1 + e2) % newHist.length,
      newHist)
  }
  init.toStream #::: go(e1, e2, init)
}

def part1(n: Int) =
  calc(0, 1, Vector(3, 7))
    .drop(n)
    .take(10)
    .mkString

def part2(initPatt: List[Int]) = {
  def go(patt: List[Int], count: Int, nums: Stream[Int]): Int = patt match {
    case Nil => count - initPatt.length
    case h :: t if h == nums.head =>
      go(t, count + 1, nums.tail)
    case _ =>
      go(initPatt, count + 1, nums.tail)
  }
  go(initPatt, 0, calc())
}

println(
  "Part 1: " + part1(503761)
)

println(
  "Part 2: " + part2(List(5,0,3,7,6,1))
)
