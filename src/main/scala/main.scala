package net.projecteuler.kurtsu

@main
def main(): Unit = {
  lazy val solutions = Problem18 ::
    Problem32 :: Problem38 ::
    Problem40 :: Problem45 ::
    Problem53 :: Problem55 :: Problem59 ::
    Problem63 :: Problem67 ::
    Problem72 :: Problem73 :: Problem80 ::
    Problem89 :: Problem90 ::
    Problem109 ::
    Problem113 :: Problem120 ::
    Problem139 ::
    Problem144 ::
    Problem233 ::
    Nil

  // too slow or read from web
  lazy val problemNumNotToInclude = List(59, 67, 89, 144, 233)

  solutions
    .filter(x => !problemNumNotToInclude.contains(x.problemNum))
    .foreach(x => println(s"problem ${x.problemNum}: ${x.solution()}"))
}
