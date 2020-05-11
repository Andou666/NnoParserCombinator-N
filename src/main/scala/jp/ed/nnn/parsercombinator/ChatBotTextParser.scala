package jp.ed.nnn.parsercombinator

import scala.util.parsing.combinator._
//例
//(chatbot
//    (reply ".*(うまくいった).*" (
//        "うまくいくなんてすごい"
//        "やったね"
//    ))
//    (reply ".*(つかれた).*" (
//        "ゆっくり休んでね"
//        "大変だったね"
//    ))
//    (reply ".*" (
//        "こんにちは"
//        "こちらこそ、こんにちは"
//        "よろしくね"
//    ))
//)

//<chat-bot> ::= "(" "chatbot" <command-list> ")"
//<command-list> ::= { <command> }
//<command> ::= <reply-command>
//<reply-command> ::= "(" "reply" <string-literal> <reply-list> ")"
//<reply-list> ::= "(" { <string-literal> } ")"
object ChatBotTextParser extends JavaTokenParsers {

  def chatBot: Parser[ChatBot] =  "(" ~ "chatbot" ~ commandList ~ ")" ^^ {t => ChatBot(t._1._2)}

  def commandList: Parser[List[Command]] = rep(command)

  def command: Parser[Command] = replyCommand | timeCommand
  //def command: Parser[Command] = replyCommand

  def replyCommand: Parser[ReplyCommand] =
    "(" ~ "reply" ~ string ~ replyList ~ ")" ^^ {t => ReplyCommand(t._1._1._2.r, t._1._2) }

  def timeCommand: Parser[TimeCommand] =
    "(" ~ "time" ~ string ~ digits ~ digits ~ string ~ replyList ~ ")" ^^
        {t => TimeCommand(
            t._1._1._1._1._1._2.r,
            t._1._1._1._1._2.toInt,
            t._1._1._1._2.toInt,
            t._1._1._2,
            t._1._2) }

  def digits : Parser[String] = "[0-9]+".r

  def replyList: Parser[List[String]] = "(" ~ rep(string) ~ ")" ^^ { t => t._1._2}

  def string: Parser[String] =  stringLiteral ^^ { s => s.substring(1, s.length - 1) }

  def apply(input: String): ParseResult[ChatBot] = parseAll(chatBot, input)


}

// 確認用
//  ChatBotTextParser("(chatbot (reply \".*\" (\"こんにちは\" \"よろしく\")))")