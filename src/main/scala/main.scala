package net.projecteuler.kurtsu

@main
def main(): Unit = {
  val solutions: List[Solution] = Problem40 ::
    Nil
  
  solutions
    .foreach(x => println(s"problem ${x.problemNum}: ${x.solution()}"))
}
