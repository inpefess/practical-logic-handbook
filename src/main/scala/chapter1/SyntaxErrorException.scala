package chapter1

case class SyntaxErrorException(message: String) extends RuntimeException {
  override def getMessage: String = message
}
