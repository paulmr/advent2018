package advent

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

object Advent17 {

  def main(args: Array[String]) = {
    val s = State().put(-10, -20, Fountain)
    println(s.get(-10, -20))
    // val s = State()
    // println(s.put(5, 5, Clay).put(5, 5, StillWater).get(5, 5))
  }

}

// class State(rows: Vector[Vector[Item]] {
//   def get(x: Int, y: Int): Item = {

// }
