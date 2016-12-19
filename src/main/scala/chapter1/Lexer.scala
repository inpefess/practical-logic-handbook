package chapter1

import chapter1.TokenType.{TokenType, charType, _}

object Lexer {
  def toTokens(line: String): Vector[String] =
    if (line.isEmpty) Vector()
    else {
      val charList = line.toCharArray.toList
      val firstChar = charList.head
      toTokensRec(charList.tail, Vector(), firstChar.toString, charType(firstChar))
    }

  private def toTokensRec(line: List[Char], acc: Vector[String], token: String, tokenType: TokenType): Vector[String] =
    line match {
      case head :: tail =>
        val newTokenType = charType(head)
        if (newTokenType != Punctuation &&
          (newTokenType == tokenType || newTokenType == Numeric && tokenType == Alphanumeric)
        )
          toTokensRec(tail, acc, token :+ head, tokenType)
        else if (tokenType == Space)
          toTokensRec(tail, acc, head.toString, newTokenType)
        else
          toTokensRec(tail, acc :+ token, head.toString, newTokenType)
      case Nil =>
        if (tokenType == Space) acc
        else acc :+ token
    }
}
