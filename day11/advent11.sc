case class Advent11(serial: Int, xmax: Int = 300, ymax: Int = 300) {
  def powerOf(x: Int, y: Int) = {
    val rack = x + 10
    val power = ((rack * y) + serial) * rack
    ((power / 100) % 10) - 5
  }

  private lazy val image = {
    val a = Array.fill(ymax + 1)(Array.fill(xmax + 1)(0L))
    coords().foreach { case (x, y) =>
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

  def coords(lim: Int = 0) =
    (for(y <- 1 to (ymax - lim - 1); x <- 1 to (xmax - lim - 1)) yield (x, y)).iterator

  def part1 =
    coords(3).maxBy { case (x, y) => square(x, y, 3) }

  def part2 = {
    val sizes = for(sz <- (1 to xmax).iterator; (x, y) <- coords(sz)) yield (x, y, sz)
    sizes.maxBy { case (x, y, sz) => square(x, y, sz) }
  }
}

val a = Advent11(8199)
println(a.part1)
println(a.part2)
