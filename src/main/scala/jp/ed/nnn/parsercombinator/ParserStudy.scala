package jp.ed.nnn.parsercombinator

object ParserStudy {

  // --- 『実用的なパーサの型』と同じ ---
  sealed trait ParseResult[+T]
  case class Success[+T](value: T, next: String) extends ParseResult[T]
  case object Failure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]
  // -----------------------------------

  def trueParser: Parser[Boolean] = input =>
    if (input.startsWith("true")) {
      Success(true, input.substring("true".length))
    } else {
      Failure
    }
}