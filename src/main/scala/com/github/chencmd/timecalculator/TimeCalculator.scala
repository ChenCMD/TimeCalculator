package com.github.chencmd.timecalculator

import com.raquo.laminar.api.L.*
import org.scalajs.dom

@main
def main(): Unit = {
  val root = dom.document.getElementById("app")
  render(root, TimeCalculator())
}

def TimeCalculator() = {
  val expr   = Var("01:00 + 02:12 - 00:29")
  val format = Var(ResultFormat.HM)
  mainTag(
    h1(
      "Time Calculator",
      a(
        img(src := "public/github-mark.svg", width := "0.9em", height := "0.9em", padding := "0 0.5em"),
        href := "https://github.com/ChenCMD/TimeCalculator"
      )
    ),
    div(
      input(
        `type`          := "text",
        size <-- expr.signal.map(s => Math.max(s.length - 2, 10)),
        controlled(
          value <-- expr.signal,
          onInput.mapToValue.filter("^[+-=0-9:smhd ]*$".r.matches) --> expr.writer
        )
      ),
      span("=", padding := "0 0.5em"),
      select(
        ResultFormat.values.map { fmt =>
          option(
            fmt.displayName,
            value := fmt.ordinal.toString,
            selected <-- format.signal.map(_ == fmt)
          )
        },
        onChange.mapToValue.map(s => ResultFormat.fromOrdinal(s.toInt)) --> format.writer
      )
    ),
    div(
      child.text <-- {
        for {
          rawExpr <- expr.signal
          format  <- format.signal
        } yield ExpressionParser.parse(rawExpr) match {
          case Right(expr) => s"$rawExpr = ${format.use(expr.eval)}"
          case Left(err)   => s"err: $err"
        }
      }
    )
  )
}
