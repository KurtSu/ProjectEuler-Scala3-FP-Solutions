package net.projecteuler.kurtsu

@main
def main(): Unit = {
  val solutions: List[Solution] = Problem40 ::
    Problem73 ::
    Problem89 ::
    Nil

  solutions
    .foreach(x => println(s"problem ${x.problemNum}: ${x.solution()}"))
}
