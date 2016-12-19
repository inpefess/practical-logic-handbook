package chapter1

import org.scalatest.FunSuite

class TokenTypeTest extends FunSuite {
  test("testCharType") {
    assert(TokenType.charType('+') == TokenType.Symbolic)
    assert(TokenType.charType('1') == TokenType.Numeric)
    assert(TokenType.charType('A') == TokenType.Alphanumeric)
    assert(TokenType.charType(' ') == TokenType.Space)
    assert(TokenType.charType(',') == TokenType.Punctuation)
    // this character is Cyrillic
    assertThrows[UnknownCharacterException](TokenType.charType('с'))
  }
}
