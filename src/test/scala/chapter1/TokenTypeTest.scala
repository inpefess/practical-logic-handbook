package chapter1

import org.scalatest.FunSuite

import scala.util.{Failure, Success, Try}

class TokenTypeTest extends FunSuite {
  test("testCharType") {
    assert(TokenType.charType('+') == TokenType.Symbolic)
    assert(TokenType.charType('1') == TokenType.Numeric)
    assert(TokenType.charType('A') == TokenType.Alphanumeric)
    assert(TokenType.charType(' ') == TokenType.Space)
    assert(TokenType.charType(',') == TokenType.Punctuation)
    // this character is Cyrillic
    Try(TokenType.charType('с')) match {
      case Success(_) => fail()
      case Failure(e) => assert(e.getMessage == "Unknown character: с")
    }
  }
}
