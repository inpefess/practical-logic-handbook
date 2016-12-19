package chapter1

object PrettyPrinter {
  def prettyPrint(expression: Expression, precedenceLevel: Int): String = expression match {
    case Mul(left, right) =>
      if (precedenceLevel <= 2)
        prettyPrint(left, 2) + " * " + prettyPrint(right, 2)
      else "(" + prettyPrint(left, 2) + " * " + prettyPrint(right, 2) + ")"
    case Add(left, right) =>
      if (precedenceLevel <= 1)
        prettyPrint(left, 1) + " + " + prettyPrint(right, 1)
      else "(" + prettyPrint(left, 1) + " + " + prettyPrint(right, 1) + ")"
    case Const(value) => value.toString
    case Var(name) => name
  }
}