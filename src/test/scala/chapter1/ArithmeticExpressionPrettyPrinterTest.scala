package chapter1

import org.scalatest.FunSuite

class ArithmeticExpressionPrettyPrinterTest extends FunSuite {
  test("testPrettyPrint") {
    val input = "((1 + 2) * 3) ^ 4"
    assert(ArithmeticExpressionParser.parse(input).mkString == input)
  }
}
