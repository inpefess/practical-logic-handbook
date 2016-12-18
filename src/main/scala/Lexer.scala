import TokenType._

object Lexer {
  private def toTokensRec(
                           line: List[Char], acc: Vector[(String, TokenType)], token: String, tokenType: TokenType
                         ): Vector[(String, TokenType)] =
    line match {
      case head :: tail =>
        val newTokenType = charType(head)
        if (newTokenType == tokenType
          || newTokenType == Numeric && tokenType == Alphanumeric
        )
          toTokensRec(tail, acc, token :+ head, tokenType)
        else
          toTokensRec(tail, acc :+ (token, tokenType), head.toString, newTokenType)
      case Nil =>
        if (token.isEmpty) acc
        else acc :+ (token, tokenType)
    }

  def toTokens(line: String): Vector[(String, TokenType)] =
    if (line.isEmpty) Vector()
    else {
      val charList = line.toCharArray.toList
      val firstChar = charList.head
      toTokensRec(charList.tail, Vector(), firstChar.toString, charType(firstChar))
    }
}
