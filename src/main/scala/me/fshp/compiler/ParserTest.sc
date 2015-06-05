import me.fshp.compiler.LexicalAnalyzer

val program =
  """
    |program
    |a = 1 + 2
    |end.
  """.stripMargin

val result = LexicalAnalyzer.parseSource("1*2+2+3+4").mkString
result