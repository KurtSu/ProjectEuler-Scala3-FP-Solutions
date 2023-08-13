package net.projecteuler.kurtsu

@main
def main(): Unit = {
  lazy val solutions: List[Solution] = Problem40 ::
    Problem53 ::
    Problem73 ::
    Problem89 :: Problem90 ::
    Problem113 ::
    Problem233 ::
    Nil

  lazy val problemNumNotToInclude = List(89, 233)

  solutions
    .filter(x => !problemNumNotToInclude.contains(x.problemNum))
    .foreach(x => println(s"problem ${x.problemNum}: ${x.solution()}"))
}
