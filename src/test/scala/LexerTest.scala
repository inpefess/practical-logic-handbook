import TokenType._
import org.scalatest.{BeforeAndAfter, FunSuite}

class LexerTest extends FunSuite with BeforeAndAfter {
  test("testToTokens") {
    val math = "2*((var_1 + x’) + 11)"
    val mathResult = Lexer.toTokens(math)
    assert(mathResult.equals(Vector(
      ("2", Numeric), ("*", Symbolic), ("((", Punctuation), ("var_1", Alphanumeric), (" ", Space), ("+", Symbolic),
      (" ", Space), ("x’", Alphanumeric), (")", Punctuation), (" ", Space), ("+", Symbolic), (" ", Space),
      ("11", Numeric), (")", Punctuation))
    ))
    println(math + " is " + mathResult.mkString(","))
    println("OK")
    println
    val cpp = "if (*p1-- == *p2++) then f() else g()"
    val cppResult = Lexer.toTokens(cpp)
    assert(cppResult == Vector(
      ("if", Alphanumeric), (" ", Space), ("(", Punctuation), ("*", Symbolic), ("p1", Alphanumeric), ("--", Symbolic),
      (" ", Space), ("==", Symbolic), (" ", Space), ("*", Symbolic), ("p2", Alphanumeric), ("++", Symbolic),
      (")", Punctuation), (" ", Space), ("then", Alphanumeric), (" ", Space), ("f", Alphanumeric), ("()", Punctuation),
      (" ", Space), ("else", Alphanumeric), (" ", Space), ("g", Alphanumeric), ("()", Punctuation)
    ))
    println(cpp + " is " + cppResult.mkString(","))
    println("OK")
    println
  }
}
