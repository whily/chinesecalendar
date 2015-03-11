/**
 * Historically accurate date.
 *
 * @author  Yujian Zhang <yujian{dot}zhang[at]gmail(dot)com>
 *
 * License: 
 *   GNU General Public License v2
 *   http://www.gnu.org/licenses/gpl-2.0.html
 * Copyright (C) 2015 Yujian Zhang
 */

package net.whily.chinesecalendar

/**
  * HistDate is an immutable date-time object that represents a date,
  * often viewed as year-month-day. Time and timezone information are
  * not stored. Other date fields, such as day-of-year, day-of-week and
  * week-of-year, are not available either. 
  * 
  * Historically accurate date is implemented from 45 BCE forward to
  * current day. This is a hybrid Gregorian/Julian calendar system on
  * October 15, 1582. Before this date, the proleptic Julian calendar
  * (proleptic means extending indefinitely) until 1 Mar. CE 4. The
  * Julian calendar has leap years every four years, whereas the
  * Gregorian has special rules for 100 and 400 years.
  * 
  * Before 45 BCE, Proleptic Julian calendar is used, assuming BCE 49
  * is leap year.
  * 
  * The problem with GregorianCalendar in Java and/or GJChronology in
  * Joda-time is that proleptic Julian is used even before 1 Mar. CE
  * 4, which makes historical work inaccurate before that date. This
  * class handles irregular Julian years according to Scaliger
  * approach in http://en.wikipedia.org/wiki/Julian_calendar#Leap_year_error. 
  * This class can work correctly back to 2 Jan. 45 CE (the first Julian day).
  * 
  * Julian/Gregorian cutover date is 15 October 1582.
  * 
  * class HistDate is thread-safe and immutable. The equals method should
  * be used for comparison.
  * 
  * @param year       1 BCE is input and returnmed as 0, 2 BCE as -1, and so on.
  * @param month      January corresponds to 1, February to 2, and so on.
  * @param dayOfMonth the 1st day as 1, the 2nd day as 2, and so on. It must be valid
  *                   for the year and month, otherwise an exception will be thrown.
  */
case class HistDate(val year: Int, val month: Int, val dayOfMonth: Int) {
  // TODO: check validity of dayOfMonth given the year and month.
  if (!((-44 <= year) && (1 <= month) && (month <= 12)
    && (1 <= dayOfMonth) && (dayOfMonth <= 31))) {
    throw new IllegalArgumentException("HistDate: illegal arguments.")
  }

  /** Equals method. */
  override def equals(other: Any): Boolean = other match {
    case that: HistDate => year == that.year && month == that.month &&
      dayOfMonth == that.dayOfMonth
    case _ => false
  }

  /** Checks if the year is a leap year. */
  def isLeapYear(): Boolean = {
    if (year <= 0) {
      if (LeapYearsBCE.contains(year)) true
      else if ((year <= -48) && (year % 4 == 0)) true // Proleptic Julian calendar
      else false
    } else if (year <= 1582) { // Julian calendar.
      if (year == 4) false
      else if (year % 4 == 0) true
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
    if (isLeapYear()) MonthDaysLeap(month - 1)
    else MonthDaysNonLeap(month - 1)
  }

  /** Returns the first day of next month. */
  private def firstDayNextMonth() = {
    if (month == 12) HistDate(year + 1, 1, 1)
    else HistDate(year, month + 1, 1)
  }

  /** Returns the last day of previous month. */
  private def lastDayPreviousMonth() = {
    if (month == 1) HistDate(year - 1, 12, 31)
    else HistDate(year, month - 1, HistDate(year, month - 1, 1).monthDays())
  }

  /** 
    * Returns a copy of this HistDate with the specified number of days added. 
    * 
    * @param daysToAdd can be either positive or negative.
    */
  def plusDays(daysToAdd: Int): HistDate = {
    // Current implementation is not efficient if daysToAdd is large
    // (e.g. when daysToAdd corresponds to many years).

    if (daysToAdd == 0)
      return this

    // Handle Julian/Gregorian clalendar cutover. In October, 4
    // October 1582 was followed by 15 October 1582.
    if ((year == 1582) && (month == 10)) {
      if ((dayOfMonth <= 4) && (dayOfMonth + daysToAdd > 4)) {
        return HistDate(1582, 10, 15).plusDays(daysToAdd - (5 - dayOfMonth))
      } else if ((dayOfMonth >= 15) && (dayOfMonth + daysToAdd < 15)) {
        return HistDate(1582, 10, 4).plusDays(daysToAdd - (14 - dayOfMonth))
      }
    }

    if (daysToAdd > 0) {
      if (dayOfMonth + daysToAdd <= monthDays()) {
        HistDate(year, month, dayOfMonth + daysToAdd)
      } else {
        firstDayNextMonth.plusDays(daysToAdd - (monthDays() - dayOfMonth) - 1)
      }
    } else {
      if (dayOfMonth + daysToAdd >= 1) {
        HistDate(year, month, dayOfMonth + daysToAdd)
      } else {
        lastDayPreviousMonth.plusDays(daysToAdd + dayOfMonth)
      }
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

  // Leap years in BCE, according to Scaliger. Assume BCE 1 is
  // represented as 0. 
  private val LeapYearsBCE = Set(-41, -38, -35, -32, -29, -26, -23, -20, -17, -14, -11, -8)

  private val MonthDaysLeap    = Array(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  private val MonthDaysNonLeap = Array(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)    
}
