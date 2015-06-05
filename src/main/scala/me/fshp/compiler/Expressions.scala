package me.fshp.compiler

trait Expressions {
  sealed abstract class Expression {
    def mkString: String = mkString(0)
    def mkString(depth: Int): String = {
      "\t" * depth + toString
    }
  }

  sealed case class ExprList(e: Expression, el: List[Expression]) extends Expression {
    override def mkString(depth: Int): String = {
      val eS = e.mkString(depth+1)
      val elS = el map {_.mkString(depth+2)} mkString ("\n")
      val shift = "\t" * depth
      val shift2 = "\t" * (depth + 1)
      shift + "ExprList(\n" +
        eS + "\n" +
        shift2 +"List(\n" +
          elS + "\n" +
        shift2 + ")\n" +
        shift + ")"
    }
  }
  sealed case class IntegerLiteral(i: Int) extends Expression
  sealed case class Sum(e:Expression) extends Expression
  sealed case class Sub(e: Expression) extends Expression
  sealed case class Mul(e: Expression) extends Expression
  sealed case class Div(e: Expression) extends Expression
}
