package advent2018

import scala.io.Source

case class Point(x: Int, y: Int) {
  def `-`(other: Point) = Math.abs(x - other.x) + Math.abs(y - other.y)

  def up    = Point(x, y - 1)
  def down  = Point(x, y + 1)
  def left  = Point(x - 1, y)
  def right = Point(x + 1, y)

  lazy val ns = Seq(up, down, left, right)
}

class Advent06(input: Seq[Point]) {

  val start = input.sortBy(p => p.x + p.y).head

  def closest(p0: Point) = {
    val dists = input.map(p0 - _).zipWithIndex.sortBy(_._1)
    if(dists(0)._1 == dists(1)._1) -1 else dists(0)._2
  }

  // def findTop(l: Seq[Point], start: Point = Point(5, 54)): Point = {
  //   val next = start.up.left
  //   val diff = l map { p => (p - start) - (p - next) }
  //   if(diff.exists(_ > 0)) findTop(l, next) else start
  // }

  def infinite(a: Point, b: Point) = (input map { p => (p - a) - (p - b) }).forall(_ < 0)

  def sizeOf(start: Point) = {
    val seen = scala.collection.mutable.Set.empty[Point]
    def go(p: Point): Int = {
      if(seen.contains(p)) 0 else {
        seen += p
        val cl = closest(p)
        if(cl == -1) 0 else {
          val ns = p.ns.filter(n => closest(n) == cl)
          if(ns.exists(n => infinite(p, n))) -1 else {
            ns.map(go).sum + 1
          }
        }
      }
    }
    go(start)
  }
}

object Advent06 {
  def main(args: Array[String]): Unit = {
    val fname = args.headOption.getOrElse("example.txt")
    val Pat = "([0-9]+), ([0-9]+)".r
    val input = Source.fromFile(fname).getLines.map {
      case Pat(x, y) => Point(Integer.parseInt(x), Integer.parseInt(y))
    }.toList

    val a = new Advent06(input)

    def expect(p: Point, exp: Int) = {
      val actual = a.sizeOf(p)
      assert(actual == exp, s"$p exp: $exp, actual: $actual")
    }

    // expect(Point(5, 5), 17)
    // expect(Point(3, 4),  9)
    // expect(Point(1, 1), -1)

    // println(
    //   (for(y <- (0 to 9); x <- (0 to 9)) yield {
    //     val cl = a.closest(Point(x, y))
    //     if(cl == -1) '.' else ('a' + cl).toChar
    //   }).grouped(10).map(_.mkString).mkString("\n")
    // )

    println(
      input.map(a.sizeOf(_)).sorted.reverse.head
    )

    // println(start)
    // println(
    //   (for(y <- (0 to 9); x <- (0 to 9)) yield {
    //     val c = closest(Point(x,y))
    //     if(c == -1) '.' else ('a' + c).toChar
    //   }).grouped(10).map(_.mkString).mkString("\n")
    // )
  }
}
