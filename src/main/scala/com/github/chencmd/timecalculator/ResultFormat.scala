package com.github.chencmd.timecalculator

enum ResultFormat(val displayName: String) {
  case D    extends ResultFormat("%d days")
  case H    extends ResultFormat("%h hours")
  case M    extends ResultFormat("%m minutes")
  case S    extends ResultFormat("%s seconds")
  case DH   extends ResultFormat("%d days %h hours")
  case HM   extends ResultFormat("%hh:%mm")
  case MS   extends ResultFormat("%mm:%ss")
  case DHM  extends ResultFormat("%d days %hh:%mm")
  case HMS  extends ResultFormat("%hh:%mm:%ss")
  case DHMS extends ResultFormat("%d days %hh:%mm:%ss")

  def use(time: Time): String = {
    val sign = if (time.isPositive) "" else "-"
    def formatUnitWithPlural(n: BigInt, unit: String): String = if (n <= 1) s"${n} ${unit}" else s"${n} ${unit}s"
    this match {
      case D    => sign + formatUnitWithPlural(time.toDays(), "day")
      case H    => sign + formatUnitWithPlural(time.toHours(true), "hour")
      case M    => sign + formatUnitWithPlural(time.toMinutes(true), "minute")
      case S    => sign + formatUnitWithPlural(time.toSeconds(true), "second")
      case DH   => sign + formatUnitWithPlural(time.toDays(), "day") + " " + formatUnitWithPlural(time.toHours(), "hour")
      case HM   => f"${sign}${time.toHours(true)}%02d:${time.toMinutes()}%02d"
      case MS   => f"${sign}${time.toMinutes(true)}%02d:${time.toSeconds()}%02d"
      case DHM  => f"${sign}${formatUnitWithPlural(time.toDays(), "day")} ${time.toHours()}%02d:${time.toMinutes()}%02d"
      case HMS  => f"${sign}${time.toHours(true)}%02d:${time.toMinutes()}%02d:${time.toSeconds()}%02d"
      case DHMS => f"${sign}${formatUnitWithPlural(time.toDays(), "day")} ${time.toHours()}%02d:${time.toMinutes()}%02d:${time.toSeconds()}%02d"
    }
  }
}
