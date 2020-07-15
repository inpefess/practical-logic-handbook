package chapter1

import org.scalatest.funsuite.AnyFunSuite

class ArithmeticExpressionTest extends AnyFunSuite {
  test("partlyEvaluate") {
    assert(Sub(Pow(Add(Mul(Neg(Const(2)), Var("x")), Var("y")), Const(3)), Var("z"))
      .partlyEvaluate(Map("x" -> -3, "y" -> 4)) == Sub(Const(1000), Var("z")))
  }
  test("simplify") {
    assert(Add(Mul(Add(Mul(Const(0), Var("x")), Neg(Const(-1))), Const(3)), Const(12)).simplify == Const(15))
  }
  test("simplify power") {
    assert(Pow(Add(Var("x"), Const(0)), Const(1)).simplify == Var("x"))
    assert(Pow(Const(1), Var("x")).simplify == Const(1))
    assert(Pow(Const(0), Var("x")).simplify == Const(0))
  }
  test("simplify subtraction") {
    assert(Add(Sub(Sub(Var("x"), Var("x")), Const(0)), Var("y")).simplify == Var("y"))
    assert(Sub(Var("x"), Sub(Var("x"), Const(0))).simplify == Const(0))
  }
  test("nothing to simplify") {
    assert(Add(Const(2), Sub(Var("x"), Var("y"))).simplify == Add(Const(2), Sub(Var("x"), Var("y"))))
  }
  test("simplify with identity") {
    assert(Mul(Mul(Pow(Const(1), Const(100)), Var("x")), Const(1)).simplify == Var("x"))
  }
  test("simplify with zero") {
    assert(Pow(Const(0), Pow(Var("z"), Mul(Var("y"), Const(0)))).simplify == Const(0))
    assert(Pow(Const(0), Const(0)).simplify == Const(1))
  }
  test("mkString") {
    assert(Var("x").mkString == "x")
  }
  test("simplify negation") {
    assert(Sub(Var("x"), Neg(Neg(Var("x")))).simplify == Const(0))
    assert(Add(Neg(Add(Var("x"), Var("y"))), Neg(Sub(Var("x"), Var("y")))).simplify ==
      Add(Add(Neg(Var("x")), Neg(Var("y"))), Add(Neg(Var("x")), Var("y")))
    )
  }
}
