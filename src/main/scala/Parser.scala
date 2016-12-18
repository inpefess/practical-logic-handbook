class SyntaxErrorException(message: String) extends RuntimeException

object Parser {
  def parse(formula: String): Expression = {
    val (parsed, not_parsed) = parseSum(Lexer.toTokens(formula))
    not_parsed match {
      case Vector() => parsed
      case _ => throw new SyntaxErrorException("Syntax error")
    }
  }

  private def parseSum(tokens: Vector[String]): (Expression, Vector[String]) =
    parseProduct(tokens) match {
      case (left, "+" +: right) =>
        val parsedRight = parseSum(right)
        (Add(left, parsedRight._1), parsedRight._2)
      case (left, right) => (left, right)
    }

  private def parseProduct(tokens: Vector[String]): (Expression, Vector[String]) =
    parseAtom(tokens) match {
      case (left, "*" +: right) =>
        val parsedRight = parseProduct(right)
        (Mul(left, parsedRight._1), parsedRight._2)
      case (left, right) => (left, right)
    }

  private def parseAtom(tokens: Vector[String]): (Expression, Vector[String]) =
    tokens match {
      case Vector() => throw new SyntaxErrorException("Unexpected end of input")
      case "(" +: right =>
        val parsedRight = parseSum(right)
        parsedRight._2 match {
          case ")" +: afterParentheses => (parsedRight._1, afterParentheses)
          case _ => throw new SyntaxErrorException("No closing parenthesis")
        }
      case some +: right =>
        TokenType.charType(some.charAt(0)) match {
          case TokenType.Alphanumeric => (Var(some), right)
          case TokenType.Numeric => (Const(some.toInt), right)
          case _ => throw new SyntaxErrorException("Syntax error")
        }
    }
}
