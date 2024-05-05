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
    this match {
      case D    => s"${time.toDays()} days"
      case H    => s"${time.toHours(true)} hours"
      case M    => s"${time.toMinutes(true)} minutes"
      case S    => s"${time.toSeconds(true)} seconds"
      case DH   => s"${time.toDays()} days ${time.toHours()} hours"
      case HM   => f"${time.toHours(true)}%02d:${time.toMinutes()}%02d"
      case MS   => f"${time.toMinutes(true)}%02d:${time.toSeconds()}%02d"
      case DHM  => f"${time.toDays()} days ${time.toHours()}%02d:${time.toMinutes()}%02d"
      case HMS  => f"${time.toHours(true)}%02d:${time.toMinutes()}%02d:${time.toSeconds()}%02d"
      case DHMS => f"${time.toDays()} days ${time.toHours()}%02d:${time.toMinutes()}%02d:${time.toSeconds()}%02d"
    }
  }
}
