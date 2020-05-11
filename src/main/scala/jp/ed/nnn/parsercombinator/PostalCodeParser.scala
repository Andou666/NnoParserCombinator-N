package jp.ed.nnn.parsercombinator

// 郵便番号パーサー
//<digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
//<zone-code> ::= <digit> <digit> <digit>
//<town-code> ::= <digit> <digit> <digit> <digit>
//<postal-code> ::= <zone-code> "-" <town-code>
case class PostalCode(zoneCode: String, townCode: String)

object PostalCodeParser extends MyFirstCombinator {

  def digit: Parser[String] = oneOf('0' to '9')

  def zoneCode: Parser[String] = map(combine(combine(digit, digit), digit), {
    t: ((String, String), String) => t._1._1 + t._1._2 + t._2
  })

  def townCode: Parser[String] = map(combine(combine(combine(digit, digit), digit), digit), {
    t: (((String, String), String), String) => t._1._1._1 + t._1._1._2 + t._1._2 + t._2
  })

  def apply(input: String): ParseResult[PostalCode] = map(combine(combine(zoneCode, s("-")), townCode), {
    t: ((String, String), String) => PostalCode(t._1._1, t._2)
  })(input)

}

// REPL確認用
// import jp.ed.nnn.parsercombinator._
// PostalCodeParser("**")