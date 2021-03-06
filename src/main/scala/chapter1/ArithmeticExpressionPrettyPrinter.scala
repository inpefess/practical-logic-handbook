package chapter1

object ArithmeticExpressionPrettyPrinter {
  def prettyPrint(expression: ArithmeticExpression, precedenceLevel: Int): String = expression match {
    case Neg(expr) =>
      if (precedenceLevel <= 4)
        "-" + prettyPrint(expr, 5)
      else "(-" + prettyPrint(expr, 5) + ")"
    case Pow(base, exponent) =>
      if (precedenceLevel <= 3)
        prettyPrint(base, 4) + " ^ " + prettyPrint(exponent, 3)
      else "(" + prettyPrint(base, 4) + " ^ " + prettyPrint(exponent, 3) + ")"
    case Mul(left, right) =>
      if (precedenceLevel <= 2)
        prettyPrint(left, 2) + " * " + prettyPrint(right, 2)
      else "(" + prettyPrint(left, 2) + " * " + prettyPrint(right, 2) + ")"
    case Add(left, right) =>
      if (precedenceLevel <= 1)
        prettyPrint(left, 1) + " + " + prettyPrint(right, 1)
      else "(" + prettyPrint(left, 1) + " + " + prettyPrint(right, 1) + ")"
    case Sub(subtrahend, minuend) =>
      if (precedenceLevel <= 1)
        prettyPrint(subtrahend, 1) + " - " + prettyPrint(minuend, 2)
      else "(" + prettyPrint(subtrahend, 1) + " - " + prettyPrint(minuend, 2) + ")"
    case Const(value) => value.toString
    case Var(name) => name
  }
}
