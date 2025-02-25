package com.github.chencmd.timecalculator

case class Time private (seconds: BigInt) {
  def isPositive: Boolean                      = 0 <= seconds
  def toDays(): BigInt                         = (seconds / Time.DAYS_SEC).abs
  def toHours(over: Boolean = false): BigInt   = ((if over then seconds else seconds % Time.DAYS_SEC) / Time.HOURS_SEC).abs
  def toMinutes(over: Boolean = false): BigInt = ((if over then seconds else seconds % Time.HOURS_SEC) / Time.MINUTES_SEC).abs
  def toSeconds(over: Boolean = false): BigInt = ((if over then seconds else seconds % Time.MINUTES_SEC)).abs
}

object Time {
  val DAYS_SEC    = 24 * 60 * 60
  val HOURS_SEC   = 60 * 60
  val MINUTES_SEC = 60

  def apply(seconds: BigInt): Time                            = new Time(seconds)
  def apply(d: BigInt, h: BigInt, m: BigInt, s: BigInt): Time = Time(d * DAYS_SEC + h * HOURS_SEC + m * MINUTES_SEC + s)
}
