import scala.io.Source

val nums =
  Source
    .fromFile("input01.txt")
    .getLines
    .map(Integer.parseInt)
    .toList

def part1 = nums.sum

def part2 = {
  def numsInf:Stream[Int] =
    nums.toStream #::: numsInf

  @annotation.tailrec
  def rec(
    nums:Stream[Int]=numsInf,
    seen:Set[Int] = Set(0),
    acc:Int = 0): Int =
  {
    val nxt = acc + nums.head
    if(seen contains nxt)
      nxt
    else
      rec(nums.tail, seen + nxt, nxt)
  }

  rec()
}

println("Part 1: " + part1)
println("Part 2: " + part2)
