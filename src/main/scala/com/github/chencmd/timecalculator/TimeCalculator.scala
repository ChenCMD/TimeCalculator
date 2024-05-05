package com.github.chencmd.timecalculator

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import com.raquo.laminar.api.L.*
import org.scalajs.dom

@js.native @JSImport("/public/github-mark.svg", JSImport.Default)
val githubMark: String = js.native

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
        img(src := githubMark, width := "0.9em", height := "0.9em", paddingLeft := "1em"),
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
    div("AST: ", child.text <-- expr.signal.map(ExpressionParser.parse(_).toString)),
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
