import me.fshp.compiler.LexicalAnalyzer


val result = LexicalAnalyzer.parseSource("a = 2<<3<<4+5-6*3*(4-6) //123")
val a = 1