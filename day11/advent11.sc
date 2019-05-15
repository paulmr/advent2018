case class Advent11(serial: Int, xmax: Int = 300, ymax: Int = 300) {
  def powerOf(x: Int, y: Int) = {
    val rack = x + 10
    val power = ((rack * y) + serial) * rack
    ((power / 100) % 10) - 5
  }

  private lazy val image = {
    val a = Array.fill(ymax + 1)(Array.fill(xmax + 1)(0L))
    for(y <- (1 to ymax); x <- (1 to xmax)) {
      a(y)(x) = powerOf(x, y) + a(y)(x-1) + a(y-1)(x) - a(y-1)(x - 1)
    }
    a.map(_.toVector).toVector
  }

  def square(x: Int, y: Int, sz: Int) = {
    val a = image(y - 1)(x - 1)
    val b = image(y - 1 + sz)(x - 1)
    val c = image(y - 1)(x - 1 + sz)
    val d = image(y - 1 + sz)(x - 1 + sz)
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
