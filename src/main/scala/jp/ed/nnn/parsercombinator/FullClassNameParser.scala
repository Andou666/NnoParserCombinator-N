package jp.ed.nnn.parsercombinator

//<grade> ::= "1" | "2" | "3"
//<class-name> ::= "A" | "B" | "C" | "D"
//<full-class-name> ::=  <grade> "年" <class-name> "組"
case class FullClassName(grade: String, className: String)

object FullClassNameParser extends MyFirstCombinator {

  def grade: Parser[String] = oneOf('1' to '3')

  def className: Parser[String] = oneOf('A' to 'D')

  def apply(input: String): ParseResult[FullClassName] =
    map(combine(combine(combine(grade, s("年")), className), s("組")), {
      t: (((String, String), String), String) => FullClassName(t._1._1._1, t._1._2)
    })(input)

}
