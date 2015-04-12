/**
 * Hyrid Julian Gregorian calendar.
 *
 * @author  Yujian Zhang <yujian{dot}zhang[at]gmail(dot)com>
 *
 * License: 
 *   GNU General Public License v2
 *   http://www.gnu.org/licenses/gpl-2.0.html
 * Copyright (C) 2015 Yujian Zhang
 */

package net.whily.chinesecalendar

object JulianGregorianCalendar {
  /** Returns the number of days in the month. */
  def monthDays(leapYear: Boolean, month: Int) = {
    if (leapYear) MonthDaysLeap(month - 1)
    else MonthDaysNonLeap(month - 1)
  }

  private val MonthDaysLeap    = Array(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  private val MonthDaysNonLeap = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)     
}

/**
  * JulianGregorianCalendar is an immutable date-time object that represents a date,
  * often viewed as year-month-day. Time and timezone information are
  * not stored. Other date fields, such as day-of-year, day-of-week and
  * week-of-year, are not available either. 
  * 
  * The calendar is essentially same as class GregorianCalendar() in
  * Java, i.e. a hybrid Gregorian/Julian calendar system with cutover
  * date as 15 October 1582. Gregorian Calendar is used after the
  * cutover while proleptic Julian calendar (proleptic means extending
  * indefinitely) is used before it.
  * 
  * Originally the target is to model irregular leap years in
  * Roman history (e.g. CE 4 is not a leap year). However such
  * ambition is abandoned:
  * 
  * 1) There is no firm conclusion on the real situation of leap year
  * error
  * 
  * 2) The main purpose of using a reference calendar (as in this
  *    class) is for easy reference. Therefore using a calendar system
  *    which has some usage scenarios is more important than
  *    accurately model Roman calendar. Propleptic Julian calendar is
  *    widely used in astronomoy to document eclipses
  *    (e.g. http://eclipse.gsfc.nasa.gov/SEhelp/calendar.html), which
  *    seems to be a more interesting choice.
  * 
  * class JulianGregorianCalendar is thread-safe and immutable. The equals method should
  * be used for comparison.
  * 
  * @param year       1 BCE is input and returned as 0, 2 BCE as -1, and so on.
  * @param month      January corresponds to 1, February to 2, and so on.
  * @param dayOfMonth the 1st day as 1, the 2nd day as 2, and so on. It must be valid
  *                   for the year and month, otherwise an exception will be thrown.
  */
case class JulianGregorianCalendar(val year: Int, val month: Int, val dayOfMonth: Int)
  extends Ordered[JulianGregorianCalendar] {
  // TODO: check validity of dayOfMonth given the year and month.
  if (!((1 <= month) && (month <= 12)
    && (1 <= dayOfMonth) && (dayOfMonth <= 31))) {
    throw new IllegalArgumentException("JulianGregorianCalendar: illegal arguments.")
  }

  /** Equals method. */
  override def equals(other: Any): Boolean = other match {
    case that: JulianGregorianCalendar => year == that.year && month == that.month &&
      dayOfMonth == that.dayOfMonth
    case _ => false
  }

  /** Checks if the year is a leap year. */
  def isLeapYear(): Boolean = {
    if (year <= 1582) { // Proleptic Julian calendar.
      if (year % 4 == 0) true
      else false
    } else { // Gregorian Calendar
      if (year % 400 == 0) true
      else if (year % 100 == 0) false
      else if (year % 4 == 0) true
      else false
    }
  }

  /** Returns the number of days in the month. */
  def monthDays() = {
    JulianGregorianCalendar.monthDays(isLeapYear(), month)
  }

  /** Returns the first day of next month. */
  private def firstDayNextMonth() = {
    if (month == 12) JulianGregorianCalendar(year + 1, 1, 1)
    else JulianGregorianCalendar(year, month + 1, 1)
  }

  /** Returns the last day of previous month. */
  private def lastDayPreviousMonth() = {
    if (month == 1) JulianGregorianCalendar(year - 1, 12, 31)
    else JulianGregorianCalendar(year, month - 1, JulianGregorianCalendar(year, month - 1, 1).monthDays())
  }

  /** 
    * Returns a copy of this JulianGregorianCalendar with the specified number of days added. 
    * 
    * @param daysToAdd can be either positive or negative.
    */
  def plusDays(daysToAdd: Int): JulianGregorianCalendar = {
    // Current implementation is not efficient if daysToAdd is large
    // (e.g. when daysToAdd corresponds to many years).

    if (daysToAdd == 0)
      return this

    // Handle Julian/Gregorian clalendar cutover. In October, 4
    // October 1582 was followed by 15 October 1582.
    if ((year == 1582) && (month == 10)) {
      if ((dayOfMonth <= 4) && (dayOfMonth + daysToAdd > 4)) {
        return JulianGregorianCalendar(1582, 10, 15).plusDays(daysToAdd - (5 - dayOfMonth))
      } else if ((dayOfMonth >= 15) && (dayOfMonth + daysToAdd < 15)) {
        return JulianGregorianCalendar(1582, 10, 4).plusDays(daysToAdd - (14 - dayOfMonth))
      }
    }

    if ((1 <= dayOfMonth + daysToAdd) && (dayOfMonth + daysToAdd <= monthDays()))
      return JulianGregorianCalendar(year, month, dayOfMonth + daysToAdd)

    if (daysToAdd > 0) {
      firstDayNextMonth.plusDays(daysToAdd - (monthDays() - dayOfMonth) - 1)
    } else {
      lastDayPreviousMonth.plusDays(daysToAdd + dayOfMonth)
    }
  }

  /** Output this date as a string, for example 1492-10-12. */
  override def toString(): String = {
    val monthDay = "年" + month + "月" + dayOfMonth + "日" 
    if (year > 0) {
      "" + year + monthDay
    } else {
      "公元前" + (1 - year) + monthDay
    }
  }

  def compare(that: JulianGregorianCalendar) = {
    val yearDiff = year - that.year
    if (yearDiff != 0) yearDiff
    else {
      val monthDiff = month - that.month
      if (monthDiff != 0) monthDiff
      else dayOfMonth - that.dayOfMonth
    }
  }

  /** Return Julian Day Number of the date starting from noon. */
  def toJdn() = {
    // Based on algorithm from
    //   http://en.wikipedia.org/wiki/Julian_day#Converting_Julian_or_Gregorian_calendar_date_to_Julian_Day_Number
    val a = (14 - month) / 12
    val y = year + 4800 - a
    val m = month + 12 * a - 3

    if (this >= JulianGregorianCalendar(1582, 10, 15)) {
      dayOfMonth + (153 * m + 2) / 5 + 365 * y + y / 4 - y / 100 + y / 400 - 32045
    } else {
      dayOfMonth + (153 * m + 2) / 5 + 365 * y + y / 4 - 32083
    }
  }

  def - (that: JulianGregorianCalendar) = toJdn() - that.toJdn()
}
