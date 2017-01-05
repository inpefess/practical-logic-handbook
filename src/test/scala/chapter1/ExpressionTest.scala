package chapter1

import org.scalatest.FunSuite

class ExpressionTest extends FunSuite {
  test("evaluate") {
    val variables = Map("x" -> 3, "y" -> 4)
    val expression = Pow(Add(Mul(Const(2), Var("x")), Var("y")), Const(3))
    assert(expression.evaluate(variables) == 1000)
  }
  test("simplify") {
    val expression = Add(Mul(Add(Mul(Const(0), Var("x")), Const(1)), Const(3)), Const(12))
    assert(expression.simplify == Const(15))
  }
  test("simplify power") {
    val expression = Pow(Add(Var("x"), Const(0)), Const(1))
    assert(expression.simplify == Var("x"))
  }
}
