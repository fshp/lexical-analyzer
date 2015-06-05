import me.fshp.compiler.LexicalAnalyzer

val program =
  """
    |program
    |a = 1 + 2
    |end.
  """.stripMargin

val result = LexicalAnalyzer.parseSource("1-(2+3)-3-4*4/4/4*2")
val a = 1