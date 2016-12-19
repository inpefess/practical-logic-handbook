import org.scalatest.FunSuite

class PrettyPrinterTest extends FunSuite {
  test("testPrettyPrint") {
    val input = "((1 + 2) + 3) + 4"
    val output = "1 + 2 + 3 + 4"
    assert(PrettyPrinter.prettyPrint(Parser.parse(input), 0) == output)
    println(input + " is " + output)
    println("OK")
    println
  }
}
