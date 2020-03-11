object const {
  val SIZE = 1000
}

sealed trait Item

case object Fountain extends Item
case object Clay extends Item
case object Sand extends Item
case object StillWater extends Item
case object FlowingWater extends Item

case class Axis[T](pos: Vector[T] = Vector.empty, neg: Vector[T] = Vector.empty) {
  def get(idx: Int) = if(idx < 0) neg(Math.abs(idx)) else pos(idx)
  def put(idx: Int, value: T) = if(idx < 0)
    copy(neg = neg.updated(Math.abs(idx), value))
  else
    copy(pos = pos.updated(idx, value))
}

object Axis {
  def apply[T](default: => T, size: Int): Axis[T] = Axis(pos = Vector.fill(size)(default), neg = Vector.fill(size)(default))
}

case class State(rows: Axis[Axis[Item]] = Axis(Axis(Sand : Item, const.SIZE), const.SIZE)) {
  def get(x: Int, y: Int) = rows.get(y).get(x)
  def put(x: Int, y: Int, value: Item) = copy(rows = rows.put(y, rows.get(y).put(x, value)))
}

object Advent17 extends AdventApp {
  type Input = State

  lazy val DoubleRange = """(-?[0-9]+)\.\.(-?[0-9]+)""".r

  def parseRange(s: String) = s match {
    case DoubleRange(min, max) =>
      min.toInt to max.toInt
    case _ => s.toInt to s.toInt
  }


  def getInput() = {
    val Line = """^([xy])=([0-9.]+)\s*, ([xy])=([0-9.]+)""".r
    scala.io.Source.fromFile("day17/example.txt").getLines
      .foldLeft(State()) { (acc, line) =>
        println(s"[$line]")
        line match {
          case Line(xy1, left, xy2, right) =>
            val (xRange, yRange) =
              if(xy1 == "x") (left, right) else (right, left)
            xRange.zip(yRange).foldLeft(acc) { (acc, xy) =>
              acc.put(xy._1, xy._2, Clay)
            }
          case _ => acc
        }
    }
  }


  def part1(input: State) = ()
  def part2(input: State) = ()
}

// class State(rows: Vector[Vector[Item]] {
//   def get(x: Int, y: Int): Item = {

// }
