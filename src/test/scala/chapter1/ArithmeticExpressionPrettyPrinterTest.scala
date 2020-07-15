package chapter1

import org.scalatest.funsuite.AnyFunSuite

class ArithmeticExpressionPrettyPrinterTest extends AnyFunSuite {
  test("test PrettyPrint 1") {
    assert(
      ArithmeticExpressionPrettyPrinter.prettyPrint(
        Pow(
          Mul(Add(Var("x"), Const(2)), Add(Mul(Const(3), Const(6)), Var("x"))),
          Sub(Mul(Const(2), Var("z")), Sub(Const(4), Var("y")))
        )
        , 1) == "((x + 2) * (3 * 6 + x)) ^ (2 * z - (4 - y))"
    )
  }
  test("test PrettyPrint 2") {
    assert(
      ArithmeticExpressionPrettyPrinter.prettyPrint(
        Sub(
          Pow(Add(Var("x"), Const(2)), Pow(Pow(Const(3), Const(6)), Var("x"))),
          Add(Add(Const(2), Var("z")), Sub(Const(4), Var("y")))
        )
        , 1) == "(x + 2) ^ (3 ^ 6) ^ x - (2 + z + 4 - y)"
    )
  }
  test("PrettyPrint negation") {
    assert(
      ArithmeticExpressionPrettyPrinter.prettyPrint(
        Sub(Var("x"), Neg(Neg(Var("x")))), 1) == "x - -(-x)"
    )
  }
}
