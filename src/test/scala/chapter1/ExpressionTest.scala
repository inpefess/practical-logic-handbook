package chapter1

import org.scalatest.FunSuite

class ExpressionTest extends FunSuite {
  test("evaluate 1") {
    val variables = Map("x" -> 3, "y" -> 4)
    val expression = Add(Mul(Const(2), Var("x")), Var("y"))
    assert(expression.evaluate(variables) == 10)
  }

  test("simplify") {
    val expression = Add(Mul(Add(Mul(Const(0), Var("x")), Const(1)), Const(3)), Const(12))
    assert(expression.simplify == Const(15))
  }
}
