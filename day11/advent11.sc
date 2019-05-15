case class Advent11(serial: Int, xmax: Int = 300, ymax: Int = 300) {
  def powerOf(x: Int, y: Int) = {
    val rack = x + 10
    val power = ((rack * y) + serial) * rack
    ((power / 100) % 10) - 5
  }

  private val _image = scala.collection.mutable.Map.empty[(Int, Int), Long]

  def image(x: Int, y: Int): Long = _image.get(x -> y).getOrElse {
    if(x < 1 || y < 1 || x > xmax || y > ymax) 0L else {
      val res = powerOf(x, y) + image(x-1, y) + image(x,y-1) - image(x - 1, y - 1)
      _image += ((x -> y) -> res)
      res
    }
  }

  def square(x: Int, y: Int, sz: Int) = {
    val a = image(x - 1, y - 1)
    val b = image(x - 1, y - 1 + sz)
    val c = image(x - 1 + sz, y - 1)
    val d = image(x - 1 + sz, y - 1 + sz)
    a + (d - b - c)
  }

  def part1 = {
    val (x, y) = (for {
      y <- (1 to (ymax - 3 - 1))
      x <- (1 to (xmax - 3 - 1))
    } yield {
      (x -> y)
    }).maxBy { case (x, y) => square(x, y, 3) }
    s"$x,$y"
  }

  def part2 = {
    val (x, y, sz, _) = (for {
      sz <- (1 to xmax)
      y  <- (1 to (ymax - sz - 1))
      x  <- (1 to (xmax - sz - 1))
    } yield {
      (x, y, sz, square(x, y, sz))
    }).maxBy(_._4)
    s"$x,$y,$sz"
  }
}

val a = Advent11(8199)

println(a.part1)
println(a.part2)
