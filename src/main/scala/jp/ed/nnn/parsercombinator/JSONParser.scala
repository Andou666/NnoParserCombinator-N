package jp.ed.nnn.parsercombinator

// JSONパーサー
//<value> ::= <obj> | <arr> | <stringLiteral> | <floatingPointNumber> | "null" | "true" | "false"
//<obj> ::= "{" [<members>] "}"
//<arr> ::= "[" [<values>] "]"
//<members> ::= <member> { "," <member> }
//<member> ::= <stringLiteral> ":" <value>
//<values> ::= <value> { "," <value> }

//追記： ホワイトスペースがある場合でも対応
object JSONParser extends Combinator {

  def obj: Parser[Map[String, Any]]  =
    (ss("{") ~> repsep(member, ss(",")) <~ ss("}") ^^ { Map() ++ _ }) <~ spacing
    //s("{") ~> repsep(member, s(",")) <~ s("}") ^^ { Map() ++ _ }

  def arr: Parser[List[Any]] =
    (ss("[") ~> repsep(value, ss(",")) <~ ss("]"))  <~ spacing
    //s("[") ~> repsep(value, s(",")) <~ s("]")

  def member: Parser[(String, Any)] =
    ((stringLiteral <~ spacing) ~ ss(":") ~ value ^^ { t => (t._1._1, t._2) }) <~ spacing
    //stringLiteral ~ s(":") ~ value ^^ { t => (t._1._1, t._2) }

  def value: Parser[Any] =
    obj <~ spacing|
      arr <~ spacing|
      stringLiteral <~ spacing|
      (floatingPointNumber ^^ { _.toDouble }) <~ spacing|
      s("null") ^^  { _ => null } |
      s("true") ^^  { _ => true } |
      s("false") ^^  { _ => false }

  def apply(input: String): Any = value(input)

}

// 確認用
// JSONParser("[1.0,-2.0,true,false,null,{\"hoge\":\"fuga\",\"piyo\":null}]")

// ホワイトスペースあり
// JSONParser("[1.0, -2.0, true, false, null, {\"hoge\" : \"fuga\", \"piyo\" : null}]")