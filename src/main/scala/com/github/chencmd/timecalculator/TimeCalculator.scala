package com.github.chencmd.timecalculator

import scala.scalajs.js
import scala.scalajs.js.annotation.JSImport

import com.raquo.laminar.api.L.*
import org.scalajs.dom

@js.native @JSImport("/github-mark.svg", JSImport.Default)
val githubMark: String = js.native

@main
def main(): Unit = {
  val root = dom.document.getElementById("app")
  render(root, TimeCalculator())
}

def TimeCalculator() = {
  val expr   = Var("2:22 + 1d2h3m - (9h03m - 3h)")
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
      p("時刻の加減算を行う自分用の雑なツールだよ"),
      p("時刻の形式は以下のようなものが使えるよ"),
      table {
        def styledTD(text: String) = td(
          text,
          textAlign   := "end",
          padding     := "0em 1em",
          border      := "solid black",
          borderWidth := "0 1px"
        )

        val examples = List(
          "1:15"     -> "1 時間 15 分",
          "1:02:3"   -> "1 時間 2 分 3 秒",
          "1d"       -> "1 日",
          "2d5h3s"   -> "2 日 5 時間 3 秒",
          "1d2h3m4s" -> "1 日 2 時間 3 分 4 秒"
        )
        examples.map { case (input, desc) => tr(styledTD(input), styledTD(desc)) }
      },
      margin  := "2.5em 0"
    ),
    div(
      input(
        `type`          := "text",
        size <-- expr.signal.map(s => Math.max(s.length - 2, 10)),
        controlled(
          value <-- expr.signal,
          onInput.mapToValue.filter("^[+-=0-9:smhd() ]*$".r.matches) --> expr.writer
        ),
        fontSize        := "1em"
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
        onChange.mapToValue.map(s => ResultFormat.fromOrdinal(s.toInt)) --> format.writer,
        fontSize        := "1em"
      )
    ),
    div(
      child <-- {
        for {
          rawExpr <- expr.signal
          format  <- format.signal
        } yield ExpressionParser.parse(rawExpr) match {
          case Right(expr) => div(
              span(s"$rawExpr", fontSize          := "1em"),
              span("=", padding                   := "0 0.5em"),
              span(format.use(expr.eval), padding := "0 0.5em", border := "black double", borderWidth := "0 0 5px 0")
            )
          case Left(err)   => s"err: $err"
        }
      },
      padding := "1em 0"
    ),
    margin := "0 1em",
    fontSize := "1.3em"
  )
}
