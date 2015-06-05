import me.fshp.compiler.LexicalAnalyzer

val program =
  """
    |program
    |a = 1 + 2
    |end.
  """.stripMargin

val result = LexicalAnalyzer.parseSource("-5+6 + 10*8 ///asd frg")
val a = 1