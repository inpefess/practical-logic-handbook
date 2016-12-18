case class Const(value: Int) extends Expression

case class Var(name: String) extends Expression

case class Add(left: Expression, right: Expression) extends Expression

case class Mul(left: Expression, right: Expression) extends Expression

sealed trait Expression {
  def evaluate(variables: Map[String, Int]): Int = this match {
    case Mul(left, right) => left.evaluate(variables) * right.evaluate(variables)
    case Add(left, right) => left.evaluate(variables) + right.evaluate(variables)
    case Const(value) => value
    case Var(name) => variables(name)
  }

  def mkString: String = this match {
    case Mul(left, right) => "(" + left.mkString + " * " + right.mkString + ")"
    case Add(left, right) => "(" + left.mkString + " + " + right.mkString + ")"
    case Const(value) => value.toString
    case Var(name) => name
  }

  def simplifyStep: Expression = this match {
    case Add(Const(a), Const(b)) => Const(a + b)
    case Mul(Const(a), Const(b)) => Const(a * b)
    case Mul(expr, Const(1)) => expr
    case Mul(Const(1), expr) => expr
    case Add(expr, Const(0)) => expr
    case Add(Const(0), expr) => expr
    case Mul(_, Const(0)) => Const(0)
    case Mul(Const(0), _) => Const(0)
    case _ => this
  }

  def simplify: Expression = this match {
    case Mul(left, right) => Mul(left.simplify, right.simplify).simplifyStep
    case Add(left, right) => Add(left.simplify, right.simplify).simplifyStep
    case _ => this
  }
}
