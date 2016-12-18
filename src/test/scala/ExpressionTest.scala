import org.scalatest.FunSuite

class ExpressionTest extends FunSuite {
  test("testEvaluate") {
    println("testEvaluate")
    val variables = Map("x" -> 3, "y" -> 4)
    print("if ")
    println(variables.mkString(","))
    val expression = Add(Mul(Const(2), Var("x")), Var("y"))
    print("then ")
    val result = expression.evaluate(variables)
    assert(result == 10)
    println(expression.mkString + " = " + result.toString)
    println("OK")
    println
  }

  test("testSimplify") {
    println("testSimplify")
    val expression = Add(Mul(Add(Mul(Const(0), Var("x")), Const(1)), Const(3)), Const(12))
    val result = expression.simplify
    assert(result == Const(15))
    println(expression.mkString + " = " + result.mkString)
    println("OK")
    println
  }
}
