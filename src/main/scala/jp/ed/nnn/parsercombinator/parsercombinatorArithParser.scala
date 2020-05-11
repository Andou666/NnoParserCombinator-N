package jp.ed.nnn.parsercombinator

//<expr> ::= <term> { "+" <term> | "-" <term> }
//<term> ::= <factor> { "*" <factor> | "/" <factor> }
//<factor> ::= <floatingPointNumber> | "(" <expr> ")"
object ArithParser extends Combinator {

  def expr: Parser[Any] = term ~ rep(s("+") ~ term | s("-") ~ term)

  def term: Parser[Any] = factor ~ rep(s("*") ~ factor | s("/") ~ factor)

  def factor: Parser[Any] = floatingPointNumber | s("(") ~ expr ~ s(")")

  def apply(input: String): Any = expr(input)

}

// 確認用
// ArithParser("(3.4+3*2.3)*4/1.3-3")