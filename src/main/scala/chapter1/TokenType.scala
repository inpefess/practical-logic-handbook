package chapter1

class UnknownCharacterException(message: String) extends RuntimeException

object TokenType extends Enumeration {
  type TokenType = Value
  val Space, Punctuation, Symbolic, Numeric, Alphanumeric = Value

  def charType(c: Char): TokenType =
    // here the order of if's matters
    if(" \t\n\r".contains(c)) Space else
    if("()[]{},".contains(c)) Punctuation else
    if("~‘!@#$%^&*-+=|\\:;<>.?/".contains(c)) Symbolic else
    if("0123456789".contains(c)) Numeric else
    // this is called alphanumeric and does not contain any numeric characters intentionally
    if("abcdefghijklmnopqrstuvwxyz_’ABCDEFGHIJKLMNOPQRSTUVWXYZ".contains(c)) Alphanumeric else
      throw new UnknownCharacterException(s"Unknown char: $c")
}
