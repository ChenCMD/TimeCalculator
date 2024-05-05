package com.github.chencmd.timecalculator

case class Time(seconds: Int) {
  def toDays(): Int                         = seconds / Time.DAYS_SEC
  def toHours(over: Boolean = false): Int   = (if over then seconds else seconds % Time.DAYS_SEC) / Time.HOURS_SEC
  def toMinutes(over: Boolean = false): Int = (if over then seconds else seconds % Time.HOURS_SEC) / Time.MINUTES_SEC
  def toSeconds(over: Boolean = false): Int = (if over then seconds else seconds % Time.MINUTES_SEC)
}

object Time {
  val DAYS_SEC    = 24 * 60 * 60
  val HOURS_SEC   = 60 * 60
  val MINUTES_SEC = 60

  def apply(seconds: Int): Time                   = new Time(seconds)
  def apply(d: Int, h: Int, m: Int, s: Int): Time = Time(d * DAYS_SEC + h * HOURS_SEC + m * MINUTES_SEC + s)
}
