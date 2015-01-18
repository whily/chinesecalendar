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
  * Date is an immutable date-time object that represents a date,
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
  * The problem with GregorianCalendar in Java and/or GJChronology in
  * Joda-time is that proleptic Julian is used even before 1 Mar. CE
  * 4, which makes historical work inaccurate before that date. This
  * class handles irregular Julian years according to Scaliger
  * approach in http://en.wikipedia.org/wiki/Julian_calendar#Leap_year_error. 
  * This class can work correctly back to 2 Jan. 45 CE (the first Julian day).
  * 
  * class Date is thread-safe and immutable. The equals method should be used for comparison.
  * 
  * @param year       1 BCE is input and returnmed as 0, 2 BCE as -1, and so on.
  * @param month      January corresponds to 1, February to 2, and so on.
  * @param dayOfMonth the 1st day as 1, the 2nd day as 2, and so on. It must be valid
  *                   for the year and month, otherwise an exception will be thrown.
  */
case class Date(val year: Int, val month: Int, val dayOfMonth: Int) {
  /** Equals method. */
  override def equals(other: Any): Boolean = other match {
    case that: Date => year == that.year && month == that.month &&
      dayOfMonth == that.dayOfMonth
    case _ => false
  }

  /** Checks if the year is a leap year. */
  def isLeapYear(): Boolean = false

  /** Returns a copy of this Date with the specified number of days added. */
  def plusDays(daysToAdd: Int): Date = this

  /** Output this date as a string, for example 1492-10-12. */
  override def toString(): String = "" + year + "-" + month + "-" + dayOfMonth
}
