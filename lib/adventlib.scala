import java.util.Date

abstract class AdventApp extends App {

  type Input

  def getInput(): Input

  def part1(i: Input): Unit
  def part2(i: Input): Unit

  private val _input = getInput()

  private def time(f: => Unit): Long = {
    val start = new Date().getTime()
    f
    new Date().getTime() - start
  }

  val part1Time = time(part1(_input))
  val part2Time = time(part2(_input))

  println(s"-> Timings: Part 1 [${part1Time}ms]; Part 2 [${part2Time}ms]")

}
