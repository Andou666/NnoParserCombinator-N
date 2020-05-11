package jp.ed.nnn.parsercombinator

case class DigitSequence(seq: Seq[String])

// <digit> ::= "0" | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
// <digit-sequence> ::= { <digit> }
object DigitSequenceParser extends MyFirstCombinator  {

  def digit: Parser[String] = oneOf('0' to '9')

  def apply(input: String): ParseResult[DigitSequence] =
    map(rep(digit), { list: List[String] => DigitSequence(list) })(input)

}

// 確認用
// DigitSequenceParser("1904387534182351")