package jp.ed.nnn.parsercombinator

// 確認用
object ParserStudy {
  sealed trait ParseResult[+T]
  case class Success[+T](value: T, next: String) extends ParseResult[T]
  case object Failure extends ParseResult[Nothing]

  type Parser[+T] = String => ParseResult[T]

  def trueParser: Parser[Boolean] = input =>
    if (input.startsWith("true")) {
      Success(true, input.substring("true".length))
    } else {
      Failure
    }

  def falseParser: Parser[Boolean] = input =>
    if (input.startsWith("false")) {
      Success(false, input.substring("false".length))
    } else {
      Failure
    }

  def booleanParser: Parser[Boolean] = input =>
    trueParser(input) match {
      case success@Success(_, _) => success
      case Failure => falseParser(input)
    }
}

// REPLで確認
// import jp.ed.nnn.parsercombinator.ParserStudy._
// **Parser("**")