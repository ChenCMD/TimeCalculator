package com.github.chencmd.timecalculator

enum Expression {
  case Add(op1: Expression, op2: Expression)
  case Subtract(op1: Expression, op2: Expression)
  case Literal(time: Time)
}

case class Time private (seconds: Int)

object Time {
  private val DAYS_SEC    = 24 * 60 * 60
  private val HOURS_SEC   = 60 * 60
  private val MINUTES_SEC = 60

  def apply(days: Int, hours: Int, minutes: Int, seconds: Int): Time = {
    Time(days * DAYS_SEC + hours * HOURS_SEC + minutes * MINUTES_SEC + seconds)
  }
}
