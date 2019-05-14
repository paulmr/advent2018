case class Advent11(serial: Int, xmax: Int = 300, ymax: Int = 300) {
  def powerOf(x: Int, y: Int) = {
    val rack = x + 10
    val power = ((rack * y) + serial) * rack
    val res = ((power / 100) % 10) - 5
    // println(s"powerOf($x, $y) = $res")
    res
  }

  private val _image = scala.collection.mutable.Map.empty[(Int, Int), Long]

  def image(x: Int, y: Int): Long = _image.get(x -> y).getOrElse {
    val res = if(x < 1 || y < 1 || x > xmax || y > ymax) 0L else {
      val res = powerOf(x, y) + image(x-1, y) + image(x,y-1) - image(x - 1, y - 1)
      _image += ((x -> y) -> res)
      res
    }
    println(s"image($x, $y) = $res")
    res
  }

  def square(x: Int, y: Int, sz: Int) = {
    val a = image(x, y)
    val b = image(x + sz - 1, y)
    val c = image(x, y + sz - 1)
    val d = image(x + sz - 1, y + sz - 1)
    println(s"a = $a, b = $b, c = $c, d = $d")
    a + d - b - c
  }
}

val examples = List(
  (122,79,57,-5),
  (217,196,39,0),
  (101,153,71,4)
)

for((x,y,serial,exp) <- examples) {
  assert(Advent11(serial).powerOf(x,y) == exp, s"[$x, $y, ($serial)] = ${Advent11(serial).powerOf(x,y)}")
}

val a = Advent11(18)

println(a.square(2,2,2))
// println(a.square(90,269,16))

// for(y <- (1 to 3)) {
//   for(x <- (1 to 3)) {
//     println(s"($x, $y) = ${a.powerOf(x, y)}")
//   }
// }
