package me.fshp.compiler

import org.scalatest._
import LexicalAnalyzer._

class LexicalAnalyzerTest extends FlatSpec with Matchers{

  "The parser" should "parse an integer literal" in {
    LexicalAnalyzer.parseSource("66613") should be (
      ExprList(IntegerLiteral(66613), List()))
  }

//  it should "parse an sum of expressions" in {
//    LexicalAnalyzer.parseSource("1+2") should be (
//      Sum(
//        IntegerLiteral(1),
//        IntegerLiteral(2)
//      )
//    )
//  }
//
//  it should "parse an diff of expressions" in {
//    LexicalAnalyzer.parseSource("1-2") should be (
//      Diff(
//        IntegerLiteral(1),
//        IntegerLiteral(2)
//      )
//    )
//  }

//  it should "parse an sum/diff of expressions" in {
//    LexicalAnalyzer.parseSource("0+1-2") should be (
//      Sum(
//        IntegerLiteral(0),
//        Diff(IntegerLiteral(1)
//      )
//    )
//  }

}
