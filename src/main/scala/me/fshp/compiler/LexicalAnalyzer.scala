package me.fshp.compiler

import java.beans.Expression

import scala.util.parsing.combinator.{PackratParsers, RegexParsers}

trait LexicalAnalyzer extends RegexParsers with Expressions {
  //val program = "program"
  //val programEnd = "end."
  //val number = "[0-9]+".r
  //val varName = "[a-Z,_]+".r
  //val begin  = "begin"
  //val end = "end"
  //val delimiter = ";"
  //val enter: Parser[Any] = program ~ block ~ programEnd
  //val block: Parser[Any] = ???
  //lazy val expr: Parser[Any]   = term ~ opt(("+" | "-") ~ expr)
  //lazy val term: Parser[Any]   = factor ~ rep(("*" | "/") ~ factor)
  //lazy val factor: Parser[Any] = number | "(" ~ expr ~ ")"

  def expr: Parser[Expression] = (term ~ rep(("+" | "-") ~ term)) ^^ {
    case t ~ p => p.foldLeft(t) {
      (e: Expression, lexeme: ~[String, Expression]) =>
        lexeme match {
          case "+" ~ r => Sum(e, r)
          case "-" ~ r => Sub(e, r)
        }
    }
  }
  
  def term: Parser[Expression] = factor ~ rep(("*" | "/") ~ factor) ^^ {
    case t ~ p => p.foldLeft(t) {
      (e: Expression, lexeme: ~[String, Expression]) =>
        lexeme match {
          case "*" ~ r => Mul(e, r)
          case "/" ~ r => Div(e, r)
        }
    }
  }

  def factor: Parser[Expression] = integer | "(" ~> expr <~ ")"

  def integer: Parser[IntegerLiteral] = "[0-9]+".r ^^ {
    case s => IntegerLiteral(s.toInt)
  }

}

object LexicalAnalyzer extends LexicalAnalyzer {
  def parseSource(source: String): Expression = {
    parseAll(expr, source) match {
      case Success (t, _) => t
      case NoSuccess(msg, next) => throw new IllegalArgumentException(next.pos.longString + ": " + msg)
    }
  }
}