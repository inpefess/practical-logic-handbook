package chapter1

object ArithmeticExpressionParser {
  def parse(formula: String): ArithmeticExpression = {
    val (parsed, not_parsed) = parseSum(Lexer.toTokens(formula))
    not_parsed match {
      case Vector() => parsed
      case _ => throw SyntaxErrorException("Syntax error")
    }
  }

  private def parseSum(tokens: Vector[String]): (ArithmeticExpression, Vector[String]) =
    parseProduct(tokens) match {
      case (left, "+" +: right) =>
        val parsedRight = parseSum(right)
        (Add(left, parsedRight._1), parsedRight._2)
      case (subtrahend, "-" +: minuend) =>
        val parsedMinuend = parseProduct(minuend)
        parsedMinuend match {
          case (nextSubtrahend, "-" +: nextMinuend) =>
            val parsedNextMinuend = parseSum(nextMinuend)
            (Sub(Sub(subtrahend, nextSubtrahend), parsedNextMinuend._1), parsedNextMinuend._2)
          case (left, "+" +: right) =>
            val parsedRight = parseSum(right)
            (Add(Sub(subtrahend, left), parsedRight._1), parsedRight._2)
          case _ =>
            val parsedRight = parseSum(minuend)
            (Sub(subtrahend, parsedRight._1), parsedRight._2)
        }
      case (left, right) => (left, right)
    }

  private def parseProduct(tokens: Vector[String]): (ArithmeticExpression, Vector[String]) =
    parsePower(tokens) match {
      case (left, "*" +: right) =>
        val parsedRight = parseProduct(right)
        (Mul(left, parsedRight._1), parsedRight._2)
      case (left, right) => (left, right)
    }

  private def parsePower(tokens: Vector[String]): (ArithmeticExpression, Vector[String]) =
    parseAtom(tokens) match {
      case (left, "^" +: right) =>
        val parsedRight = parsePower(right)
        (Pow(left, parsedRight._1), parsedRight._2)
      case (left, right) => (left, right)
    }

  private def parseAtom(tokens: Vector[String]): (ArithmeticExpression, Vector[String]) =
    tokens match {
      case Vector() => throw SyntaxErrorException("Unexpected end of input")
      case "(" +: right =>
        val parsedRight = parseSum(right)
        parsedRight._2 match {
          case ")" +: afterParentheses => (parsedRight._1, afterParentheses)
          case _ => throw SyntaxErrorException("No closing parenthesis")
        }
      case "-" +: right =>
        val parsedRight = parseSum(right)
        (Neg(parsedRight._1), parsedRight._2)
      case some +: right =>
        TokenType.charType(some.charAt(0)) match {
          case TokenType.Alphanumeric => (Var(some), right)
          case TokenType.Numeric => (Const(some.toInt), right)
          case _ => throw SyntaxErrorException("Syntax error")
        }
    }
}
