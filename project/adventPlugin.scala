import sbt._

/* this is loopy */

object AdventPlugin extends AutoPlugin {

  override def extraProjects = {
    val libProj = Project("lib", new File("lib"))
    val days = for {
      dir <- (PathFinder(new File(".")) * "day*").get
      if !(dir * "*.scala").get.isEmpty
    } yield Project(dir.getName(), dir).dependsOn(libProj)
    libProj :: days.toList
  }

}
