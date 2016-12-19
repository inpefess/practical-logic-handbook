package chapter1

import org.scalatest.{BeforeAndAfter, FunSuite}

class LexerTest extends FunSuite with BeforeAndAfter {
  test("testToTokens") {
    val math = "2*((var_1 + x’) + 11)"
    val mathResult = Lexer.toTokens(math)
    assert(mathResult.equals(Vector("2", "*", "(", "(", "var_1", "+", "x’", ")", "+", "11", ")")))
    println(math + " is " + mathResult.mkString(","))
    println("OK")
    println
    val cpp = "if (*p1-- == *p2++) then f() else g()"
    val cppResult = Lexer.toTokens(cpp)
    assert(cppResult == Vector("if", "(", "*", "p1", "--", "==", "*", "p2", "++", ")", "then", "f", "(", ")", "else",
      "g", "(", ")"))
    println(cpp + " is " + cppResult.mkString(","))
    println("OK")
    println
    // the last character is Cyrillic
    val unknownCharacter = "x + у"
    println("Cyrillic characters are not allowed")
    assertThrows[UnknownCharacterException](Lexer.toTokens(unknownCharacter))
    println("OK")
    println
  }
}
