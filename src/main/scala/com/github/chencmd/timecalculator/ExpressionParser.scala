package com.github.chencmd.timecalculator

import scala.util.parsing.combinator.RegexParsers

object ExpressionParser {
  def parse(input: String): Either[String, Expression] = new ExpressionParser().parseExpression(input)
}

class ExpressionParser extends RegexParsers {
  override def skipWhitespace: Boolean = false

  def parseExpression(input: String): Either[String, Expression] = parseAll(expression, input) match {
    case Success(res, _) => Right(res)
    case Failure(msg, _) => Left(msg)
    case Error(msg, _)   => Left(msg)
  }

  lazy val expression: Parser[Expression] = for {
    left <- term
    expr <- (binaryOperator ~ term).* ^^ { opTerms =>
      opTerms.foldLeft(left) { case (l, (op ~ r)) => op(l, r) }
    }
  } yield expr

  lazy val binaryOperator: Parser[(Expression, Expression) => Expression] =
    "+" ^^^ Expression.Add.apply | "-" ^^^ Expression.Subtract.apply

  lazy val term: Parser[Expression] = allowWhiteSpace((time ^^ Expression.Literal.apply) | ("(" ~> expression <~ ")"))

  lazy val time: Parser[Time] = simpleTime | complexTime

  lazy val complexTime: Parser[Time] = for {
    day    <- (integer <~ "d").?
    hour   <- (integer <~ "h").?
    minute <- (integer <~ "m").?
    second <- (integer <~ "s").?
    if day.isDefined || hour.isDefined || minute.isDefined || second.isDefined
  } yield Time(day.getOrElse(0), hour.getOrElse(0), minute.getOrElse(0), second.getOrElse(0))

  lazy val simpleTime: Parser[Time] = for {
    (hour ~ _ ~ minute) <- integer ~ ":" ~ integer
  } yield Time(0, hour, minute, 0)

  lazy val integer: Parser[Int] = """-?\d+""".r ^^ { _.toInt }

  def singletonLit(s: String): Parser[s.type] = s ^^^ s

  def allowWhiteSpace[T](s: Parser[T]): Parser[T] = whiteSpace.* ~> s <~ whiteSpace.*
}
