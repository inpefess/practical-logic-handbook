import TokenType._
import org.scalatest.FunSuite

class TokenTypeTest extends FunSuite {
  test("testCharType") {
    assert(charType('+') == Symbolic)
    assert(charType('1') == Numeric)
    assert(charType('A') == Alphanumeric)
    assert(charType(' ') == Space)
    assert(charType(',') == Punctuation)
    // this character is Cyrillic
    assertThrows[UnknownCharacterException](charType('с'))
  }
}
