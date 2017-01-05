package chapter1

sealed trait ArithmeticExpression {
  def evaluate(variables: Map[String, Int]): Int = this match {
    case Mul(left, right) => left.evaluate(variables) * right.evaluate(variables)
    case Add(left, right) => left.evaluate(variables) + right.evaluate(variables)
    case Pow(base, exponent) => math.pow(base.evaluate(variables).toDouble, exponent.evaluate(variables).toDouble).toInt
    case Sub(subtrahend, minuend) => subtrahend.evaluate(variables) - minuend.evaluate(variables)
    case Const(value) => value
    case Var(name) => variables(name)
  }

  def mkString: String = ArithmeticExpressionPrettyPrinter.prettyPrint(this, 1)

  def simplify: ArithmeticExpression = this match {
    case Mul(left, right) => Mul(left.simplify, right.simplify).simplifyStep
    case Add(left, right) => Add(left.simplify, right.simplify).simplifyStep
    case Pow(base, exponent) => Pow(base.simplify, exponent.simplify).simplifyStep
    case Sub(subtrahend, minuend) => Sub(subtrahend.simplify, minuend.simplify).simplifyStep
    case _ => this
  }

  protected def simplifyStep: ArithmeticExpression = this match {
    case Add(Const(a), Const(b)) => Const(a + b)
    case Add(expr, Const(0)) => expr
    case Add(Const(0), expr) => expr
    case Sub(Const(a), Const(b)) => Const(a - b)
    case Sub(expr, Const(0)) => expr
    case Sub(subtrahend, minuend) =>
      if (subtrahend == minuend)
        Const(0)
      else
        Sub(subtrahend, minuend)
    case Mul(Const(a), Const(b)) => Const(a * b)
    case Mul(_, Const(0)) => Const(0)
    case Mul(Const(0), _) => Const(0)
    case Mul(expr, Const(1)) => expr
    case Mul(Const(1), expr) => expr
    case Pow(_, Const(0)) => Const(1)
    case Pow(Const(0), _) => Const(0)
    case Pow(expr, Const(1)) => expr
    case Pow(Const(1), _) => Const(1)
    case _ => this
  }
}

case class Const(value: Int) extends ArithmeticExpression

case class Var(name: String) extends ArithmeticExpression

case class Add(left: ArithmeticExpression, right: ArithmeticExpression) extends ArithmeticExpression

case class Sub(subtrahend: ArithmeticExpression, minuend: ArithmeticExpression) extends ArithmeticExpression

case class Mul(left: ArithmeticExpression, right: ArithmeticExpression) extends ArithmeticExpression

case class Pow(base: ArithmeticExpression, exponent: ArithmeticExpression) extends ArithmeticExpression
