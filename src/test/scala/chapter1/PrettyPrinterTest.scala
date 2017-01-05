package chapter1

import org.scalatest.FunSuite

class PrettyPrinterTest extends FunSuite {
  test("testPrettyPrint") {
    val input = "((1 + 2) * 3) ^ 4"
    assert(PrettyPrinter.prettyPrint(Parser.parse(input), 1) == input)
  }
}
