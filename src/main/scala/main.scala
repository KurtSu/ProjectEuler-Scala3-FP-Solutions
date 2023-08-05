package net.projecteuler.kurtsu

@main
def main(): Unit = {
  lazy val solutions: List[Solution] = Problem40 ::
    Problem73 ::
    Problem89 :: Problem90 ::
    Nil

  lazy val problemNumNotToInclude = List(89)
  
  solutions
    .filter(x => !problemNumNotToInclude.contains(x.problemNum))
    .foreach(x => println(s"problem ${x.problemNum}: ${x.solution()}"))
}
