package chapter1

sealed trait ArithmeticExpression {
  def partlyEvaluate(variables: Map[String, Int]): ArithmeticExpression =
    substituteVariables(variables).simplify

  def substituteVariables(variables: Map[String, Int]): ArithmeticExpression =
    this match {
      case Mul(a, b) => Mul(a.substituteVariables(variables), b.substituteVariables(variables))
      case Add(a, b) => Add(a.substituteVariables(variables), b.substituteVariables(variables))
      case Pow(a, b) => Pow(a.substituteVariables(variables), b.substituteVariables(variables))
      case Sub(a, b) => Sub(a.substituteVariables(variables), b.substituteVariables(variables))
      case Var(name) =>
        variables.get(name) match {
          case Some(const) => Const(const)
          case None => Var(name)
        }
      case Const(const) => Const(const)
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
    case Pow(Const(a), Const(b)) => Const(math.pow(a.toDouble, b.toDouble).toInt)
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
