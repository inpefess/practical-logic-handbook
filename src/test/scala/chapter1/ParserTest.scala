package chapter1

import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  test("parse simple sum") {
    assert(Parser.parse("x + 1") == Add(Var("x"), Const(1)))
  }
  test("parse complex input") {
    assert(
      Parser.parse("(x1 + x2 + x3) * (1 + 2 + 3 * x + y)") ==
        Mul(Add(Var("x1"), Add(Var("x2"), Var("x3"))),
          Add(Const(1), Add(Const(2), Add(Mul(Const(3), Var("x")), Var("y"))))
        )
    )
  }
  test("wrong input wont parse") {
    assertThrows[SyntaxErrorException](Parser.parse("x ++ 1"))
  }
  test("parse juxtaposition") {
    assert(Parser.parse("x y+2(z+1)") ==
      Add(Mul(Var("x"),Var("y")),Mul(Const(2),Add(Var("z"),Const(1))))
    )
  }
}
