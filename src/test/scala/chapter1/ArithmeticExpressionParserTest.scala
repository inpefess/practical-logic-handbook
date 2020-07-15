package chapter1

import org.scalatest.funsuite.AnyFunSuite

import scala.util.{Failure, Success, Try}

class ArithmeticExpressionParserTest extends AnyFunSuite {
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
    Try(ArithmeticExpressionParser.parse("x ++ 1")) match {
      case Success(_) => fail()
      case Failure(e) => assert(e.getMessage == "Syntax error")
    }
  }
  test("parse power") {
    assert(ArithmeticExpressionParser.parse("x ^ y ^ z") == ArithmeticExpressionParser.parse("x ^ (y ^ z)"))
    assert(ArithmeticExpressionParser.parse("x ^ y * z") == Mul(Pow(Var("x"), Var("y")), Var("z")))
    assert(ArithmeticExpressionParser.parse("x + y ^ z") == Add(Var("x"), Pow(Var("y"), Var("z"))))
  }
  test("parse subtraction") {
    assert(ArithmeticExpressionParser.parse("x - y - z") == Sub(Sub(Var("x"), Var("y")), Var("z")))
    assert(ArithmeticExpressionParser.parse("(x - y) - z") == Sub(Sub(Var("x"), Var("y")), Var("z")))
    assert(ArithmeticExpressionParser.parse("x - (y - z)") == Sub(Var("x"), Sub(Var("y"), Var("z"))))
    assert(ArithmeticExpressionParser.parse("x - y + z") == Add(Sub(Var("x"), Var("y")), Var("z")))
    assert(ArithmeticExpressionParser.parse("(x - y) + z") == Add(Sub(Var("x"), Var("y")), Var("z")))
    assert(ArithmeticExpressionParser.parse("x - (y + z)") == Sub(Var("x"), Add(Var("y"), Var("z"))))
    assert(ArithmeticExpressionParser.parse("x + y - z") == Add(Var("x"), Sub(Var("y"), Var("z"))))
    assert(ArithmeticExpressionParser.parse("(x + y) - z") == Sub(Add(Var("x"), Var("y")), Var("z")))
    assert(ArithmeticExpressionParser.parse("x + (y - z)") == Add(Var("x"), Sub(Var("y"), Var("z"))))
  }
  test("no closing parenthesis") {
    Try(ArithmeticExpressionParser.parse("(x - y -z")) match {
      case Success(_) => fail()
      case Failure(e) => assert(e.getMessage == "No closing parenthesis")
    }
  }
  test("unexpected end of input") {
    Try(ArithmeticExpressionParser.parse("x - - - ")) match {
      case Success(_) => fail()
      case Failure(e) => assert(e.getMessage == "Unexpected end of input")
    }
  }
  test("wrong atom") {
    Try(ArithmeticExpressionParser.parse("$ - $")) match {
      case Success(_) => fail()
      case Failure(e) => assert(e.getMessage == "Syntax error")
    }
  }
  test("parse negation") {
    assert(ArithmeticExpressionParser.parse("x - - - x") == Sub(Var("x"), Neg(Neg(Var("x")))))
  }
}
