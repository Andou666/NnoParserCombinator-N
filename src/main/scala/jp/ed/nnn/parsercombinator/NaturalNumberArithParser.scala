package jp.ed.nnn.parsercombinator

case class NaturalNumberArith(num: Int, terms: List[(String, Int)])

// <digit-excluding-zero> ::= "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" | "9"
// <digit> ::= "0" | <digit-excluding-zero>
// <natural-number> ::= <digit-excluding-zero> { <digit> }
// <expr> ::= <natural-number> { "+" <natural-number> | "-" <natural-number> }
object NaturalNumberArithParser extends MyFirstCombinator {

  def digitExcludingZero: Parser[String] = oneOf('1' to '9')

  def digit: Parser[String] = select(s("0"), digitExcludingZero)

  def naturalNumber: Parser[Int] = map(combine(digitExcludingZero, rep(digit)), {
    t: (String, List[String]) => (t._1 + t._2.mkString).toInt
  })

  def apply(input: String): ParseResult[NaturalNumberArith] =
    map(combine(naturalNumber, rep(select(combine(s("+"), naturalNumber), combine(s("-"), naturalNumber)))), {
      t: (Int, List[(String, Int)]) => NaturalNumberArith(t._1, t._2)
    })(input)

}

// 確認用
// NaturalNumberArithParser("74+12+362-94")