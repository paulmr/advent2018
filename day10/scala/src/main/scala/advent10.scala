package advent

import org.scalajs.dom
import org.querki.jquery._
import scala.io.Source

case class Point(x: Int, y: Int) {
  def `+`(that: Point) = copy(x = x + that.x, y = y + that.y)
}

case class Star(position: Point, velocity: Point) {
  def next = copy(position = position + velocity)
}

case class Stars(stars: List[Star]) {
  override def toString = {
    (for(row <- makeGrid) yield {
      (for(star <- row) yield if(star.isDefined) "#" else ".").mkString
    }).mkString("\n")
  }
  def gridDims: (Point, Point) = {
    val minx = stars.map(_.position.x).min
    val miny = stars.map(_.position.y).min
    val maxx = stars.map(_.position.x).max
    val maxy = stars.map(_.position.y).max
    (Point(minx, miny), Point(maxx, maxy))
  }
  def makeGrid: List[List[Option[Star]]] = {
    val (max, min) = gridDims
    (for(y <- (min.y to max.y)) yield {
      (for(x <- (min.x to max.x)) yield {
        stars.find(s => s.position.x == x && s.position.y == y)
      }).toList
    }).toList
  }
  def next = copy(stars = stars.map(_.next))
  def complete(steps: Int): Stars = if(steps == 0) this else next.complete(steps - 1)
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
  def updateView(parentDom: dom.Node, stars: Stars) = {
    val parent = $(parentDom)
    parent.empty()
    for(row <- stars.makeGrid) {
      val rowElement = $("""<div class="row"></div>""")
      for(star <- row) {
        val starEl = $("""<span class="star"></span>""")
        if(star.isDefined) starEl.addClass("active")
        rowElement.append(starEl)
      }
      parent.append(rowElement)
    }
  }

  def main(args: Array[String]): Unit = ${ () =>
    val inputFile = args.headOption.getOrElse("../example.txt")
    // val stars = Stars.fromString(Source.fromFile(inputFile).mkString)
    var stars = Stars.fromString(Advent10Input.example)
    // var stars = Stars.fromString(Advent10Input.input10)

    val parent = dom.document.getElementById("canvas")
    // updateView(parent, stars)
    // dom.console.log(stars.toString)

    dom.document.getElementById("next-button").addEventListener("click", (ev: dom.Event) => {
      updateView(parent, stars)
      stars = stars.next
    })

  }
}
