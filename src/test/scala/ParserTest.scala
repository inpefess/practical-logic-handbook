import org.scalatest.FunSuite

class ParserTest extends FunSuite {
  test("testParse") {
    val simpleInput = "x + 1"
    val simpleResult = Parser.parse(simpleInput)
    assert(simpleResult == Add(Var("x"), Const(1)))
    println(simpleInput + " is " + simpleResult)
    println("OK")
    println
    val complexInput = "(x1 + x2 + x3) * (1 + 2 + 3 * x + y)"
    val complexResult = Parser.parse(complexInput)
    assert(complexResult == Mul(Add(Var("x1"), Add(Var("x2"), Var("x3"))), Add(Const(1), Add(Const(2),
      Add(Mul(Const(3), Var("x")), Var("y"))))))
    println(complexInput + " is " + complexResult)
    println("OK")
    println
    val wrongInput = "x ++ 1"
    assertThrows[SyntaxErrorException](Parser.parse(wrongInput))
    println(wrongInput + " has wrong syntax")
    println("OK")
    println
  }
}
