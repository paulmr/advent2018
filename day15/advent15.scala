import scala.io.Source

object Advent15 extends App {
  sealed abstract class Cell(val c: Char)
  case object Wall extends Cell('#')
  case object Floor extends Cell('.')
  case class Creature(
      kind: Char,
      enemy: Char,
      id: Int,
      ap: Int = 3,
      hp: Int = 200
  ) extends Cell(kind) {
    def hit(ap: Int) = copy(hp = hp - ap)
  }

  case class Point(x: Int, y: Int) {
    // 'reading' order
    def ns = List(
      copy(y = y - 1),
      copy(x = x - 1),
      copy(x = x + 1),
      copy(y = y + 1)
    )
    def i(w: Int) = y * w + x
  }

  case class State(
      map: Vector[Cell],
      w: Int,
      isComplete: Boolean = false,
      dead: Set[Int] = Set.empty
  ) {
    val h = map.length / w
    def at(p: Point) =
      if (p.x < w && p.x >= 0 && p.y < h && p.y >= 0) Some(map(p.i(w)))
      else None
    def withCell(p: Point, c: Cell) = copy(map = map.updated(p.i(w), c))
    def points = for (y <- 0 until h; x <- 0 until w) yield Point(x, y)
    def complete = copy(isComplete = true)
    def notComplete = copy(isComplete = false)
    def score =
      points
        .map(p => at(p))
        .collect {
          case Some(c: Creature) => c.hp
        }
        .sum
    def elves = points.map(p => (p, at(p))).collect {
      case (p, Some(e: Creature)) if e.c == 'E' => (p, e)
    }
  }

  def parseSource(s: Source, elvesAttackPower: Int = 3) = {
    def makeCell(c: Char, id: Int) = c match {
      case 'E' => new Creature('E', 'G', id)
      case 'G' => new Creature('G', 'E', id)
      case '#' => Wall
      case '.' => Floor
    }
    val lines = s.getLines.toList
    val w = lines.head.length
    val map =
      for ((c, id) <- lines.flatten.toVector.zipWithIndex) yield makeCell(c, id)
    State(map, w)
  }

  def findRoute(
      from: Point,
      last: Point,
      seen: Map[Point, Point]
  ): List[Point] = {
    val nxt = seen(last)
    if (nxt == from) List(last)
    else last :: findRoute(from, nxt, seen)
  }

  def findTarget(from: Point, them: Char, st: State): Option[List[Point]] = {
    def go(
        stack: List[(Int, Point)],
        seen: Map[Point, Point]
    ): Option[List[Point]] = {
      stack.sortBy {
        case (len, Point(x, y)) => (len, y, x)
      } match {
        case Nil => None /* not found */
        case (len, nxt) :: rest =>
          nxt.ns.find(p => st.at(p).exists(_.c == them)).map { found =>
            findRoute(from, nxt, seen)
          } orElse {
            val ns = nxt.ns.filter(
              n => !seen.isDefinedAt(n) && st.at(n) == Some(Floor)
            )
            go(
              ns.map(n => (len + 1, n)) ++: rest,
              seen ++ ns.map(n => n -> nxt).toMap
            )
          }
      }
    }
    go(List((0, from)), Map(from -> from))
  }

  def move(st: State, c: Creature, p: Point): (Point, State) =
    // if already next to an enemy, do nothing
    if (p.ns.exists(n => st.at(n).exists(_.c == c.enemy))) (p, st.notComplete)
    else {
      findTarget(p, c.enemy, st) match {
        case None => (p, st)
        case Some(route) =>
          val newPos = route.last
          (
            newPos,
            st.notComplete.withCell(p, Floor).withCell(newPos, st.at(p).get)
          )
      }
    }

  def attack(st: State, c: Creature, p: Point) = {
    p.ns
      .map(n => (n, st.at(n)))
      .collect {
        case (n, Some(e: Creature)) if e.c == c.enemy => (n, e)
      }
      .sortBy {
        case (Point(x, y), e) => (e.hp, y, x)
      }
      .headOption
      .map {
        case (n, e) =>
          val hit = e.hit(c.ap)
          if (hit.hp > 0)
            st.withCell(n, hit)
          else {
            st.withCell(n, Floor).copy(dead = st.dead + hit.id)
          }
      }
      .getOrElse(st)
  }

  def tick(st0: State): State = {
    st0.points.sortBy { case Point(x, y) => (y, x) }.foldLeft(st0.complete) {
      (st, p) =>
        st0.at(p) match {
          case Some(c: Creature) if !st.dead.contains(c.id) =>
            val (newPos, st1) = move(st, c, p)
            attack(st1, c, newPos)
          case _ => st
        }
    }
  }

  def part1(st: State, count: Int = 0): Unit = {
    val nxt = tick(st)
    if (nxt.isComplete) {
      val score = st.score * (count - 1)
      println(s"Part 1: $score")
    } else {
      part1(nxt, count + 1)
    }
  }

  def part2(st0: State) = {
    def recurse(st: State, count: Int = 0): Option[Int] = {
      val nxt = tick(st)
      if (st.elves.length != nxt.elves.length) None
      else if (nxt.isComplete) Some((count - 1) * st.score)
      else recurse(nxt, count + 1)
    }

    def search(ap: Int): Int = {
      val updated = st0.elves.foldLeft(st0) { (st, ep) =>
        val (p, e) = ep
        st.withCell(p, e.copy(ap = ap))
      }
      recurse(updated) getOrElse search(ap + 1)
    }

    println(s"Part 2: ${search(3)}")
  }

  val input =
    """################################
      |#############..#################
      |#############..#.###############
      |############G..G.###############
      |#############....###############
      |##############.#...#############
      |################..##############
      |#############G.##..#..##########
      |#############.##.......#..######
      |#######.####.G##.......##.######
      |######..####.G.......#.##.######
      |#####.....#..GG....G......######
      |####..###.....#####.......######
      |####.........#######..E.G..#####
      |####.G..G...#########....E.#####
      |#####....G.G#########.#...######
      |###........G#########....#######
      |##..#.......#########....##.E.##
      |##.#........#########.#####...##
      |#............#######..#.......##
      |#.G...........#####........E..##
      |#....G........G..G.............#
      |#..................E#...E...E..#
      |#....#...##...G...E..........###
      |#..###...####..........G###E.###
      |#.###########..E.......#########
      |#.###############.......########
      |#################.......########
      |##################....#..#######
      |##################..####.#######
      |#################..#####.#######
      |################################""".stripMargin

  val st = parseSource(Source.fromString(input))
  part1(st)
  part2(st)
}
