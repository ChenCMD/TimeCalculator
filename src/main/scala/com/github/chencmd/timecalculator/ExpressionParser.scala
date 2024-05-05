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

  lazy val term: Parser[Expression] = allowWhiteSpace(time | ("(" ~> expression <~ ")"))

  lazy val time: Parser[Expression.Literal] = (simpleTime | complexTime) ^^ Expression.Literal.apply

  lazy val complexTime: Parser[Time] = for {
    d <- (integer <~ "d").?
    h <- (integer <~ "h").?
    m <- (integer <~ "m").?
    s <- (integer <~ "s").?
    if d.isDefined || h.isDefined || m.isDefined || s.isDefined
  } yield Time(d.getOrElse(0), h.getOrElse(0), m.getOrElse(0), s.getOrElse(0))

  lazy val simpleTime: Parser[Time] = for {
    (hour ~ _ ~ minute) <- integer ~ ":" ~ integer
  } yield Time(0, hour, minute, 0)

  lazy val integer: Parser[BigInt] = """-?\d+""".r ^^ BigInt.apply

  def singletonLit(s: String): Parser[s.type] = s ^^^ s

  def allowWhiteSpace[T](s: Parser[T]): Parser[T] = whiteSpace.* ~> s <~ whiteSpace.*
}
