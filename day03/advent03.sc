case class Claim(id:Int,x:Int,y:Int,w:Int,h:Int)

type Fabric = Vector[Vector[Set[Int]]]

def bounds(claims:Seq[Claim]) =
{
  val xs = claims.map(c => c.x + c.w)
  val ys = claims.map(c => c.y + c.h)
  (xs.max, ys.max)
}

def makeFabric(w:Int,h:Int): Fabric =
  Vector.fill(h)(Vector.fill(w)(Set.empty[Int]))

def parse(s:String):Claim =
  s.split("[^0-9]+")
    .filter(_.length > 0)
    .map(Integer.parseInt)
    .toList match {
      case id :: x :: y :: w :: h :: Nil => Claim(id,x,y,w,h)
    }

def addClaim(fabric:Fabric, c:Claim):Fabric = {
  (c.y until (c.y + c.h)).foldLeft(fabric) { (fabric, y) =>
    val rowNew =
      (c.x until (c.x + c.w)).foldLeft(fabric(y)) { (row, x) =>
        row.updated(x, row(x) + c.id)
      }
    fabric.updated(y, rowNew)
  }
}

def distribute(cs:Seq[Claim]): Fabric = {
  val (w, h) = bounds(cs)
  cs.foldLeft(makeFabric(w, h))(addClaim)
}

def part1(fabric:Fabric) =
  fabric.map(_.count(_.toSeq.length > 1)).sum

def part2(ids:Seq[Int], fabric:Fabric) =
  fabric.foldLeft(ids.toSet) { (seen, row) =>
    row.foldLeft(seen) { (seen, cell) =>
      if(cell.toList.length > 1)
        cell.foldLeft(seen)(_ - _)
      else
        seen
    }
  }.toList.head

@main
def main() = {
  val input =
    scala.io.Source.fromFile("input03.txt")
      .getLines
      .toList
      .map(parse)
  val fabric = distribute(input)
  println(s"Part 1: ${part1(fabric)}")
  println(s"Part 2: ${part2(input.map(_.id), fabric)}")
}
