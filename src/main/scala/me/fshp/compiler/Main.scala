package me.fshp.compiler

object Main extends App {
  println(LexicalAnalyzer.parseSource("1-2-(4-3)-4"))
}
