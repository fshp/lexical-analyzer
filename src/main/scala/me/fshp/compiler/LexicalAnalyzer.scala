package me.fshp.compiler

import scala.util.parsing.combinator.RegexParsers

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

  def expr: Parser[ExprList] = term ~ operator1List ^^ {
    case (e ~ el) => ExprList(e, el)
  }
  
  def term: Parser[ExprList] = factor ~ operator2List ^^ {
    case (e ~ el) => ExprList(e, el)
  }

  def factor = integer

  def operator1List: Parser[List[Expression]] = rep(("+" | "-") ~ integer) ^^ {
    _ map {
      case ("+" ~ e) => Sum(e)
      case ("-" ~ e) => Sub(e)
    }
  }

  def operator2List: Parser[List[Expression]] = rep(("*" | "/") ~ integer) ^^ {
    _ map {
      case ("*" ~ e) => Mul(e)
      case ("/" ~ e) => Div(e)
    }
  }
//
//  def sum: Parser[Sum] = operand ~ "+" ~ operand ^^ {
//    case (l ~ _ ~ r) => Sum(l, r)
//  }

  def integer: Parser[IntegerLiteral] = "[0-9]+".r ^^ {
    s => IntegerLiteral(s.toInt)
  }

}

object LexicalAnalyzer extends LexicalAnalyzer {
  def parseSource(source: String): Expression = {
    parseAll(expr, source) match {
      case Success (t, _) => t
      case NoSuccess(msg, next) => IntegerLiteral(0) //new IllegalArgumentException(next.pos.longString + ": " + msg)
    }
  }
}