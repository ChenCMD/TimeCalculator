package com.github.chencmd.timecalculator

import scala.scalajs.js
import scala.scalajs.js.annotation.*

import org.scalajs.dom
import com.raquo.laminar.api.L.{*, given}

@main
def main(): Unit = {
  val root = dom.document.getElementById("app")
  render(root, TimeCalculator())
}

def TimeCalculator() = {
  val expr   = Var("01:00 + 02:12 - 00:29")
  val format = Var(ResultFormat.HM)
  mainTag(
    h1("Time Calculator", forId := "expression"),
    div(
      input(
        `type`          := "text",
        idAttr          := "expression",
        size <-- expr.signal.map(s => Math.max(s.length - 2, 10)),
        controlled(
          value <-- expr.signal,
          onInput.mapToValue.filter("^[+-=0-9:smhd ]*$".r.matches) --> expr.writer
        )
      ),
      span("=", padding := "0 0.5em"),
      select(
        ResultFormat.values.map(fmt => option(value := fmt.ordinal.toString, fmt.displayName)),
        onChange.mapToValue.filter("^[0-9]+$".r.matches).map(s => ResultFormat.fromOrdinal(s.toInt)) --> format.writer
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
