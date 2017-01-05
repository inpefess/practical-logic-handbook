package chapter1

import org.scalatest.FunSuite

class ArithmeticExpressionParserTest extends FunSuite {
  test("parse simple sum") {
    assert(ArithmeticExpressionParser.parse("x + 1") == Add(Var("x"), Const(1)))
  }
  test("parse complex input") {
    assert(
      ArithmeticExpressionParser.parse("(x1 + x2 + x3) * (1 + 2 + 3 * x + y)") ==
        Mul(Add(Var("x1"), Add(Var("x2"), Var("x3"))),
          Add(Const(1), Add(Const(2), Add(Mul(Const(3), Var("x")), Var("y"))))
        )
    )
  }
  test("wrong input wont parse") {
    assertThrows[SyntaxErrorException](ArithmeticExpressionParser.parse("x ++ 1"))
  }
  test("parse juxtaposition") {
    assert(ArithmeticExpressionParser.parse("x y+2(z+1)") ==
      Add(Mul(Var("x"),Var("y")),Mul(Const(2),Add(Var("z"),Const(1))))
    )
  }
  test("parse power") {
    assert(ArithmeticExpressionParser.parse("x ^ y ^ z") == ArithmeticExpressionParser.parse("x ^ (y ^ z)"))
    assert(ArithmeticExpressionParser.parse("x ^ y * z") == Mul(Pow(Var("x"), Var("y")), Var("z")))
    assert(ArithmeticExpressionParser.parse("x + y ^ z") == Add(Var("x"), Pow(Var("y"), Var("z"))))
  }
}