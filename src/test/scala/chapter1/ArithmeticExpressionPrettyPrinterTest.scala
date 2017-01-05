package chapter1

import org.scalatest.FunSuite

class ArithmeticExpressionPrettyPrinterTest extends FunSuite {
  test("testPrettyPrint") {
    val input = "((x + 2) * 3) ^ (4 - y - z)"
    assert(ArithmeticExpressionParser.parse(input).mkString == input)
  }
}
