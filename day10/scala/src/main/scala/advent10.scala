package advent

import scala.io.Source

case class Point(x: Int, y: Int) {
  def `+`(that: Point) = copy(x = x + that.x, y = y + that.y)
}

case class Star(position: Point, velocity: Point) {
  def next = copy(position = position + velocity)
}

case class Stars(stars: List[Star]) {
  override def toString = {
    val (min, max) = gridDims
    (for(y <- (min.y to max.y)) yield {
      (for(x <- min.x to max.x) yield {
        if(stars.exists(s => s.position.x == x && s.position.y == y)) "#" else "."
      }).mkString
    }).mkString("\n")
  }

  lazy val gridDims: (Point, Point) = {
    val minx = stars.map(_.position.x).min
    val miny = stars.map(_.position.y).min
    val maxx = stars.map(_.position.x).max
    val maxy = stars.map(_.position.y).max
    (Point(minx, miny), Point(maxx, maxy))
  }

  lazy val size: Point = {
    val (min, max) = gridDims
    Point(max.x - min.x, max.y - min.y)
  }

  def next = copy(stars = stars.map(_.next))
}

object Stars {
  def fromString(s: String) =
    Stars(s.split("\n").map(Star.fromString _).toList)
}

object Point {
  val Pat = """\s*(-?[0-9]+)\s*,\s*(-?[0-9]+)""".r
  def fromString(s: String): Point = s match {
    case Pat(x, y) => Point(Integer.parseInt(x), Integer.parseInt(y))
  }
}

object Star {
  val Pat = """position=<(.+)>\s+velocity=<(.+)>""".r
  def fromString(s: String): Star = s match {
    case Pat(position, velocity) => Star(Point.fromString(position), Point.fromString(velocity))
  }
}

object Advent10 {

  @annotation.tailrec
  def solve(stars: Stars, last: Stars): Stars = {
    if(stars.size.x < last.size.x && stars.size.y < last.size.y)
      solve(stars.next, stars)
    else
      last
  }

  def main(args: Array[String]): Unit = {
    val inputFile = args.headOption.getOrElse("../example.txt")
    val stars = Stars.fromString(Source.fromFile(inputFile).mkString)
    println(solve(stars.next, stars))
  }
}
