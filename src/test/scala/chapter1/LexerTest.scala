package chapter1

import org.scalatest.{BeforeAndAfter, FunSuite}

class LexerTest extends FunSuite with BeforeAndAfter {
  test("tokenize simple math") {
    assert(
      Lexer.toTokens("2*((var_1 + x’) + 11) ") ==
      Vector("2", "*", "(", "(", "var_1", "+", "x’", ")", "+", "11", ")")
    )
  }
  test("tokenize C++ like code") {
    assert(
      Lexer.toTokens("if (*p1-- == *p2++) then f() else g()") ==
        Vector("if", "(", "*", "p1", "--", "==", "*", "p2", "++", ")", "then", "f", "(", ")", "else", "g", "(", ")")
    )
  }
  test("empty line - no terms") {
    assert(Lexer.toTokens("") == Vector())
  }
}
