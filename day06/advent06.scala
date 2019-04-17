package advent2018

import scala.io.Source

case class Point(x: Int, y: Int) {
  def `-`(other: Point) = Math.abs(x - other.x) + Math.abs(y - other.y)

  lazy val ns = Seq(
    Point(x, y - 1),
    Point(x, y + 1),
    Point(x - 1, y),
    Point(x + 1, y)
  )
}

class Advent06(input: Seq[Point]) {

  def closest(p0: Point) = {
    val dists = input.map(p0 - _).zipWithIndex.sortBy(_._1)
    if(dists(0)._1 == dists(1)._1) -1 else dists(0)._2
  }

  def sumdist(p0: Point) = input.map(p0 - _).sum

  def infinite(a: Point, b: Point) = (input map { p => (p - a) - (p - b) }).forall(_ < 0)

  def sizeOf(measure: Point => Int)(pred: Point => Boolean)(start: Point) = {
    val seen = scala.collection.mutable.Set.empty[Point]
    @annotation.tailrec
    def go(ps: List[Point], acc: Int): Int = ps match {
      case Nil => acc
      case p :: rest =>
        if(seen.contains(p)) go(rest, acc)
        else {
          seen += p
          val cl = measure(p)
          if(cl == -1) go(rest, acc)
          else {
            val ns = p.ns.filter(pred)
            if(ns.exists(n => infinite(p, n))) -1
            else {
              go(ps.tail ++ ns, acc + 1)
            }
          }
        }
    }
    go(List(start), 0)
  }

  def part1 = input.map { p =>
    val cl = closest(p)
    sizeOf(closest)(closest(_) == closest(p))(p)
  }.sorted.reverse.head

  def part2 = input.map { p =>
    sizeOf(sumdist)(sumdist(_) < 10000)(p)
  }.sorted.reverse.head
}

object Advent06 {
  def main(args: Array[String]): Unit = {
    val fname = args.headOption.getOrElse("example.txt")
    val Pat = "([0-9]+), ([0-9]+)".r
    val input = Source.fromFile(fname).getLines.map {
      case Pat(x, y) => Point(Integer.parseInt(x), Integer.parseInt(y))
    }.toList

    val a = new Advent06(input)

    println(s"Part 1: ${a.part1}")
    println(s"Part 2: ${a.part2}")
  }
}
