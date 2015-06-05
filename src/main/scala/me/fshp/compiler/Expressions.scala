package me.fshp.compiler

trait Expressions {
  sealed trait Expression extends Printable
  sealed case class IntegerLiteral(i: Int) extends Expression with IntPrintable
  sealed abstract class BinaryOp(val l: Expression, val r: Expression) extends Expression with BinaryPrintable
  sealed case class Sum(override val l: Expression, override val r: Expression) extends BinaryOp(l ,r)
  sealed case class Sub(override val l: Expression, override val r: Expression) extends BinaryOp(l ,r)
  sealed case class Mul(override val l: Expression, override val r: Expression) extends BinaryOp(l ,r)
  sealed case class Div(override val l: Expression, override val r: Expression) extends BinaryOp(l ,r)


  sealed trait Printable {
    this: Expression =>
      val shifting = "\t"
      val name = this.getClass.getSimpleName
      override def toString: String = toString(0)
      def toString(depth: Int): String = shifting * depth + super.toString
  }

  sealed trait IntPrintable extends Printable {
    this: IntegerLiteral =>
      override def toString(depth: Int): String = {
        shifting * depth + "Int(" + i + ")"
      }
  }

  sealed trait BinaryPrintable extends Printable {
    this: BinaryOp =>
      override def toString(depth: Int): String = {
        shifting * depth + name + "(\n" +
          l.toString(depth + 1) + ",\n" +
          r.toString(depth + 1) + "\n" +
        shifting * depth + ")"
      }
  }
}
