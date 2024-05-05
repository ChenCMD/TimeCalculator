package com.github.chencmd.timecalculator

enum Expression {
  case Add(op1: Expression, op2: Expression)
  case Subtract(op1: Expression, op2: Expression)
  case Literal(time: Time)

  def eval: Time = this match {
    case Add(op1, op2)      => (Time(op1.eval.seconds + op2.eval.seconds))
    case Subtract(op1, op2) => (Time(op1.eval.seconds - op2.eval.seconds))
    case Literal(time)      => time
  }
}
