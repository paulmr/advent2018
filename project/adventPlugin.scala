import sbt._

/* this is loopy */

object AdventPlugin extends AutoPlugin {

  override def extraProjects =
    for {
      dir <- (PathFinder(new File(".")) * "day*").get
      if !(dir * "*.scala").get.isEmpty
    } yield Project(dir.getName(), dir)

}
