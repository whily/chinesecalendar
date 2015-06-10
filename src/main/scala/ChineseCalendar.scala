/**
 * chinese Calendar.
 *
 * @author  Yujian Zhang <yujian{dot}zhang[at]gmail(dot)com>
 *
 * License: 
 *   GNU General Public License v2
 *   http://www.gnu.org/licenses/gpl-2.0.html
 * Copyright (C) 2014-2015 Yujian Zhang
 */

package net.whily.chinesecalendar

import scala.collection.mutable    // For toMap()
import scala.util.matching.Regex

case class ChineseCalendar(era: String, year: String,
  month: String, dayOfMonth: String) {
  /** Equals method. */
  override def equals(other: Any): Boolean = other match {
    case that: ChineseCalendar => era == that.era &&
      year == that.year && month == that.month && dayOfMonth == that.dayOfMonth
    case _ => false
  }
  
  /** Return the difference between current date and start of the month. */
  def dayDiff() = {
    if (dayOfMonth == "晦") ChineseCalendar.monthLength(this) - 1
    else if (ChineseCalendar.Sexagenary.contains(dayOfMonth)) {
      val sexagenary = ChineseCalendar.findMonth(this).sexagenary
      ChineseCalendar.sexagenaryDiff(sexagenary, dayOfMonth)
    }
    else ChineseCalendar.Dates.indexOf(dayOfMonth)
  }

  /**
    * Returns a copy of this ChineseCalendar with the specified number of days added. 
    * 
    * @param daysToAdd can be either positive or negative.
    */
  def plusDays(daysToAdd: Int): ChineseCalendar = {
   // Current implementation is not efficient if daysToAdd is large
    // (e.g. when daysToAdd corresponds to many years).

    val month = ChineseCalendar.findMonth(this)

    if (daysToAdd == 0)
      return this

    val newDay = dayDiff() + 1 + daysToAdd
    val monthDays = ChineseCalendar.monthLength(this)
    if ((1 <= newDay) && (newDay <= monthDays))
      return plusDaysThisMonth(daysToAdd)

    if (daysToAdd > 0) {
      firstDayNextMonthFast().plusDays(newDay - 1 - monthDays)
    } else {
      lastDayPrevMonth(true).plusDays(newDay)
    }
  }

  def plusDaysThisMonth(daysToAdd: Int) = {
    var dom = ""
    if (ChineseCalendar.Sexagenary.contains(dayOfMonth)) {
      dom = ChineseCalendar.sexagenaryAdd(dayOfMonth, daysToAdd)
    } else {
      val date =
        if (dayOfMonth == "晦") ChineseCalendar.Dates(ChineseCalendar.monthLength(this) - 1)
        else dayOfMonth
      dom = ChineseCalendar.dateAdd(date, daysToAdd)
    }
    ChineseCalendar(era, year, month, dom)
  }

  /** Returns the first day of next month, fast in the sense that there
    * is no check on era boundary. */
  private def firstDayNextMonthFast(): ChineseCalendar = {
    val (_, months, _) = ChineseCalendar.lookupDate(this)
    val i = months.indexWhere(_.month == month)
    if (i == months.length - 1) {
      val yearNumber = ChineseCalendar.Numbers(ChineseCalendar.Numbers.indexOf(year.dropRight(1)) + 1)
      ChineseCalendar.parseDate(era + yearNumber + year.takeRight(1))
    } else {
      ChineseCalendar(era, year, months(i + 1).month, "初一")
    }
  }

  /** Returns the first day of next month, general in the sense that era
    * boundary is considered.
    * 
    * @param continuous When set to true, continuity of days is preferred over the
    *                   correctness (i.e. if two calendar systems are
    *                   not aligned, the returned date might not be
    *                   the first day of the month).
    * 
    * TODO: optimize performance so we only need one versin. */
  def firstDayNextMonth(continuous: Boolean): ChineseCalendar = {
    val Some(eraSegment) = ChineseCalendar.containingSegment(this)
    val nextChineseDate = firstDayNextMonthFast()
    val nextDate = ChineseCalendar.toDate(nextChineseDate, false)
    if (eraSegment.contains(nextDate))
      return nextChineseDate

    val nextEra = eraSegment.next
    val dates = ChineseCalendar.fromDate(nextDate)
    // TODO: remove the try/catch debug facilities once all table data is tested.
    try { 
      val Some(chineseDate) = dates.find(_.startsWith(nextEra))
      val result = ChineseCalendar.parseDate(chineseDate)
      // Handle the case that different calendar systems are used,
      // e.g. the Three Kingdoms.
      if (continuous) result
      else ChineseCalendar(result.era, result.year, result.month, "初一")
    } catch {
      case ex: Exception =>
        println("firstDayNextMonth(): " + era + year + month)
        null
    }
  }

  /** Returns the last day of previous month, considering era boundary.
    * 
    * @param continuous When set to true, continuity of days is preferred over the
    *                   correctiness (i.e. if two calendar systems are
    *                   not aligned, the returned date might not be
    *                   the last day of the month).
    * 
    * TODO: optimize performance so we only need one versin. */
  def lastDayPrevMonth(continuous: Boolean): ChineseCalendar = {
    // We handle this function different from firstDayNextMonth()
    // since the backtracking of previous month might cause index < 0.
    val Some(eraSegment) = ChineseCalendar.containingSegment(this)
    val firstDay = ChineseCalendar(era, year, month, "初一")
    val prevDate = ChineseCalendar.toDate(firstDay, true).plusDays(-1)
    val dates = ChineseCalendar.fromDate(prevDate)    
    if (eraSegment.contains(prevDate)) {
      val Some(chineseDate) = dates.find(_.startsWith(era))
      ChineseCalendar.parseDate(chineseDate)
    } else {
      val prevEra = eraSegment.prev
      // TODO: remove the try/catch debug facilities once all table data is tested.
      try {
        val Some(chineseDate) = dates.find(_.startsWith(prevEra))
        val result = ChineseCalendar.parseDate(chineseDate)
        // Handle the case that different calendar systems are used,
        // e.g. the Three Kingdoms.
        if (continuous) result
        else {
          val prevDateAdj = ChineseCalendar.toDate(ChineseCalendar(result.era, result.year, result.month, "晦"), true)
          val datesAdj = ChineseCalendar.fromDate(prevDateAdj)
          val Some(chineseDateAdj) = dates.find(_.startsWith(prevEra))
          ChineseCalendar.parseDate(chineseDateAdj)
        }
      } catch {
        case ex: Exception =>
          println("lastDayPrevMonth(): " + era + year + month)
          null
      }
    }
  }

  /** Return the date with day of month as `that`, but everything else
    * as current date.  If day of month of `that` does not exist in
    * current month, return the closest date.
    */
  private def sameDayAs(that: ChineseCalendar) = {
    val monthDays = ChineseCalendar.monthLength(this)
    if (that.dayDiff() >= monthDays - 1) {
      ChineseCalendar(era, year, month, "晦")
    } else {
      ChineseCalendar(era, year, month, that.dayOfMonth)
    }
  }

  /** Return the date of next month with same day of month as current date. 
      If this is impossible, return the closest date in next month. */
  def sameDayNextMonth() = {
    firstDayNextMonth(false).sameDayAs(this)
  }

  /** Return the date of previous month with same day of month as current date. 
      If this is impossible, return the closest date in next month. */
  def sameDayPrevMonth() = {
    lastDayPrevMonth(false).sameDayAs(this)
  }

  /** Return the sexagenary of the year. */
  def yearSexagenary() = {
    val (_, _, sexagenary) = ChineseCalendar.lookupDate(this)
    sexagenary
  }

  def normalizedYear() = if (year == "一年") "元年" else year

  def normalizedMonth() = if (month == "一月") "正月" else month

  override def toString = {
    era + normalizedYear() + normalizedMonth() + dayOfMonth
  }
}

/**
  * Chinese calendar.
  */
object ChineseCalendar {
  // For debugging purpose only. Will be removed once code is stable.
  def main(args: Array[String]) {
  }

  // 干支
  private val Sexagenary = Array(
    "甲子", "乙丑", "丙寅", "丁卯", "戊辰", "己巳", "庚午", "辛未", "壬申", "癸酉",
    "甲戌", "乙亥", "丙子", "丁丑", "戊寅", "己卯", "庚辰", "辛巳", "壬午", "癸未",
    "甲申", "乙酉", "丙戌", "丁亥", "戊子", "己丑", "庚寅", "辛卯", "壬辰", "癸巳",
    "甲午", "乙未", "丙申", "丁酉", "戊戌", "己亥", "庚子", "辛丑", "壬寅", "癸卯",
    "甲辰", "乙巳", "丙午", "丁未", "戊申", "己酉", "庚戌", "辛亥", "壬子", "癸丑",
    "甲寅", "乙卯", "丙辰", "丁巳", "戊午", "己未", "庚申", "辛酉", "壬戌", "癸亥"
  )

  def sexagenaryDiff(x: String, y: String) = {
    // Note that in Java, % is remainder, not modulo operator. So some checking is needed.
    val z = (Sexagenary.indexOf(y) - Sexagenary.indexOf(x)) % 60
    if (z < 0) z + 60
    else z
  }

  /** Return a new sexagenary by adding `add` to the original `sexagenary`.*/
  def sexagenaryAdd(sexagenary: String, add: Int) = {
    var z = (Sexagenary.indexOf(sexagenary) + add) % 60
    if (z < 0) z += 60
    Sexagenary(z)
  }

  /** Return an array of in-order sexagenaries, with `start` as the 1st
    * element, and the array has `length` elements. */
  def sexagenaries(start: String, length: Int): Array[String] = {
    assert((Sexagenary.contains(start)) && (length > 1))
    val result = new Array[String](length)
    val index = Sexagenary.indexOf(start)

    for (i <- 0 until length) {
      result(i) = Sexagenary((index + i) % 60)
    }
    result
  }

  /** Return Julian/Gregorian date given `date` in Chinese calendar. 
    * Use `check` for range check of the date. */
  def toDate(date: ChineseCalendar, check: Boolean): JulianGregorianCalendar = {
    val (firstDay, months, _) = lookupDate(date)
    if (months.indexWhere(_.month == date.month) < 0)
      throw new IllegalArgumentException("toDate(): illegal month " + date.month) 
    val (dayDiff, sexagenary) = daysFromNewYear(date.month, months)
    val dayOfMonth = date.dayOfMonth
    val result = firstDay.plusDays(dayDiff + date.dayDiff())
    if (check && !dateInRange(result, date.era))
      throw new IllegalArgumentException("toDate(): date " + result +
        " not in era " + date.era)
    result
  }

  /** Return the month information corresponding to the `date`. */
  private def findMonth(date: ChineseCalendar): Month = {
    val (_, months, _) = lookupDate(date)
    val Some(month) = months.find(_.month == date.month)
    month
  }

  /** Return the number of days of the month which the date belongs to. */
  def monthLength(date: ChineseCalendar): Int = findMonth(date).length

  /** Return the number of days of the month which the date belongs to. */
  def monthLength(date: String): Int = monthLength(parseDate(date))

  /** Return the sexagenary of the 1st day of the month. */
  def sexagenary1stDayOfMonth(date: ChineseCalendar): String =
    findMonth(date).sexagenary

  /** Return the sexagenary of the 1st day of the month. */
  def sexagenary1stDayOfMonth(date: String): String =
    sexagenary1stDayOfMonth(parseDate(date))

  // Get table information from date.
  private def lookupDate(era: String, year: String):
      (JulianGregorianCalendar, Array[Month], String) = {
    // Handling the special cases for going from BCE to CE.
    // Note that there is no 漢哀帝元壽三年, however this can happen when called
    // by sameDayNextMonth().
    // Similar handling is used for other cases.
    if ((era == "漢哀帝元壽") && (year == "三年")) {
      return lookupDate("漢平帝元始", "一年")
    }

    val yearOffset = Numbers.indexOf(year.dropRight(1)) - 1 // Remove 年

    val Year(firstDay, months, sexagenary) = eraMap(era) match {
      case (table, eraStartYear) =>
        val tableStartYear = table(0).firstDay.year // Year of the 1st entry in the table.
        var index = eraStartYear - tableStartYear + yearOffset
        // Handle the case that 太初元年 has 15 months, therefore it
        // starts in BCE 105, while ends in BCE 103. This creats a gap
        // in table indices.
        if ((table == BCEYears) && (eraStartYear > -104)) {
          index -= 1
        }
        // TODO: after all data is inputed, remove the following try.
        try {
          table(index)
        } catch {
          case ex: Exception => println("lookupDate(): " + era + year)
        }
    }

    (firstDay, months, sexagenary)
  }

  // Get table information from date.  
  private def lookupDate(date: ChineseCalendar):
      (JulianGregorianCalendar, Array[Month], String) =
    lookupDate(date.era, date.year)

  /**
    * Return the number of days in difference between the indicated
    * `month` and the 1st day of that year. Calculation is based on
    * sexagenary of the 1st day of each month. 
    */
  private def daysFromNewYear(month: String, months: Array[Month]) = {
    def days(daysDiff: Int, prevSexagenary: String, ms: Array[Month]): (Int, String) = {
      val diff = sexagenaryDiff(prevSexagenary, ms(0).sexagenary)
      // One month can only have 29 or 30 days. Value 0 is for the 1st
      // month in the year.
      if ((diff != 0) && (diff != 29) && (diff != 30)) {
        throw new RuntimeException("Month length is incorrect: " +
          prevSexagenary + "-" + ms(0).sexagenary + 
          "\n" + months.mkString(":"))
      }

      val updatedDaysDiff =  daysDiff + diff
      if (month == ms(0).month) {
        (updatedDaysDiff, ms(0).sexagenary)
      } else {
        days(updatedDaysDiff, ms(0).sexagenary, ms.tail)
      }
    }

    days(0, months(0).sexagenary, months)
  }

  /**
    * The grammar for a Chinese date is as follows:
    * 
    *   ChineseCalendar = era [year [month [dayOfMonth]]]
    * 
    * @param era This could be either the title of the monarch, 
    *            or both the title and the era (年號).
    *            For monarchs with era (except for 漢武帝 who
    *            firstly used era, but also ruled without era
    *            before the first usage), title and era
    *            are concatenated together like 漢武帝建元.
    * @param year 元年|二年|三年|...
    * @param month the grammar is: month = [春|夏|秋|冬] [閏] 正月|一月|二月|三月|...|十月|十一月|十二月
    *        Note that whether the combination of season/month is valid or not is not checked.
    * @param dayOfMonth the grammar is:
    *        dayOfMonth = Sexagenary|朔|晦|初一|初二|...|初九|初十|十一|十二|...|十九|二十|廿一|廿二|...|廿九|三十|
    */  
  def toDate(date: String, check: Boolean = true): JulianGregorianCalendar = {
    // An example of string with minimum length: 黃初元年
    if (date.length < 4) {
      throw new IllegalArgumentException("toDate(): illegal argument date: " + date)
    }
    toDate(parseDate(date), check)
  }

  /** Return a list of Chinese dates corresponding to Julian/Gregorian
    * `date`. The first element of the list is the date used more
    * primarily in history books, e.g. 《資治通鑑》.
    */
  def fromDate(date: JulianGregorianCalendar): List[String] = {
    var result: List[String] = Nil

    for (era <- containingSegments(date)) {
      if (era.contains(date)) {
        val chineseDate = era.startChinese.plusDays(date - era.start)
        result = chineseDate.toString() :: result
      }
    }

    result.sortBy(ChineseCalendar.dynastyOrder(_))
  }

  def fromDate(date: String): List[String] =
    fromDate(JulianGregorianCalendar.fromString(date))

  def parseDate(s: String): ChineseCalendar = {
    // Select the first month
    if (s.takeRight(1) == "年") {
      val (era, year) = parseYear(s)
      val (_, months, _) = lookupDate(era, year)
      return parseDate(era + year + months(0).month)
    }

    var dayOfMonth = "初一"   // Default day of month.
    var endIndex = s.length
    if (s.takeRight(1) == "朔") {
      dayOfMonth = "初一"
      endIndex -= 1
    } else if (s.takeRight(1) == "晦") {
      dayOfMonth = "晦" // Will handle later as the month lenght is not known.
      endIndex -= 1  
    } else {
      val t = s.takeRight(2)
      if (Sexagenary.contains(t) || Dates.contains(t)) {
        endIndex -= 2
        val k = s.lastIndexOf("月")
        if (k + 1 != endIndex) {
          throw new IllegalArgumentException("parseDate(): illegal argument s: " + s)
        }
        dayOfMonth = t
      }
    }

    parseMonth(s.substring(0, endIndex), dayOfMonth)
  }

  private def parseMonth(s: String, dayOfMonth: String): ChineseCalendar = {
    var month = "一月"  // Default month
    var endIndex = s.length
    if (s.endsWith("月")) {
      val k = s.lastIndexOf("年")
      if (k == -1) {
        throw new IllegalArgumentException("parseMonth(): illegal argument s: " + s)
      }
      endIndex = k + 1
      if (Array("春", "夏", "秋", "冬").contains(s.substring(k + 1, k + 2))) {
        month = s.substring(k + 2)
      } else {
        month = s.substring(k + 1)        
      }
      if (month == "正月")
        month = "一月"
    }

    parseYear(s.substring(0, endIndex), month, dayOfMonth)
  }

  private def parseYear(s: String): (String, String) = {
    if (s.takeRight(1) != "年") {
      throw new IllegalArgumentException("parseYear(): illegal argument s: " + s)
    }

    var t = s.dropRight(1)

    // Get the year in Chinese.
    var year = t.takeRight(3)
    if (!Numbers.contains(year)) {
      year = t.takeRight(2)
      if (!Numbers.contains(year)) {
        year = t.takeRight(1)
        if (year == "元") {
          year = "一"
        }
        if (!Numbers.contains(year)) {
          throw new IllegalArgumentException("parseYear(): illegal argument s: " + s)
        }
      }
    }

    val era = t.dropRight(year.length)
    if (!eraMap.contains(era)) {
      throw new IllegalArgumentException("parseYear(): illegal argument s: " + s)
    }

    (era, year + "年")
  }

  private def parseYear(s: String, month: String, dayOfMonth: String): ChineseCalendar = {
    val (era, year) = parseYear(s)
    ChineseCalendar(era, year, month, dayOfMonth)
  }

  /**
    * @param month       month name
    * @param sexagenary sexagenary (干支) of the first day of the month
    */
  case class Month(month: String, sexagenary: String) {
    /** Number of days in month. With current sexagenary based design, 
      * month length can only be known after construction. */
    var length = 0

    /** Equals method. */
    override def equals(other: Any): Boolean = other match {
      case that: Month => month == that.month && sexagenary == that.sexagenary
      case _ => false
    }
  }

  private val Numbers = Array(
    "〇", "一", "二", "三", "四", "五", "六", "七", "八", "九",
    "十", "十一", "十二", "十三", "十四", "十五", "十六", "十七", "十八", "十九",
    "二十", "二十一", "二十二", "二十三", "二十四", "二十五", "二十六", "二十七", "二十八", "二十九",
    "三十", "三十一", "三十二", "三十三", "三十四", "三十五", "三十六", "三十七", "三十八", "三十九",
    "四十", "四十一", "四十二", "四十三", "四十四", "四十五", "四十六", "四十七", "四十八", "四十九",
    "五十", "五十一", "五十二", "五十三", "五十四", "五十五", "五十六", "五十七", "五十八", "五十九",
    "六十", "六十一", "六十二", "六十三", "六十四", "六十五", "六十六", "六十七", "六十八", "六十九"
  )
  // Not so good to publish an array here. TODO: encapsulates.
  val Dates = Array(
    "初一", "初二", "初三", "初四", "初五", "初六", "初七", "初八", "初九", "初十",
    "十一", "十二", "十三", "十四", "十五", "十六", "十七", "十八", "十九", "二十",
    "廿一", "廿二", "廿三", "廿四", "廿五", "廿六", "廿七", "廿八", "廿九", "三十"
  )
  private val LeapMonth = "閏"
  private val ForwardMonth = "進"
  private val LaterMonth = "後"
  // Only used for the last three months in 漢武帝太初.
  private val SpecialLaterMonth = "後後"  

  /** Return a new date by adding `add` to the original `date`.*/
  def dateAdd(date: String, add: Int) =
    Dates(Dates.indexOf(date) + add)

  /** Return an array of months by parsing the string S, in the format of
    *   sexageneray1 sexagenary2 ...
    * If there is a leap month, then insert character 閏.
    * 
    * @param startMonthIndex the index of the first month in the string, 
    *                        1 means the first month.
    */
  def months(s: String, startMonthIndex: Int = 1): Array[Month] = {
    val words = s.trim.split(" ")
    var monthIndex = startMonthIndex
    var result: List[Month] = Nil
    var prefix = ""
    var specialFlag = false
    for (word <- words) {
      word match {
        case LeapMonth => prefix = LeapMonth
        case LaterMonth => prefix = LaterMonth
        case SpecialLaterMonth =>
          prefix = LaterMonth
          specialFlag = true
        case ForwardMonth => monthIndex += 1
        case _ =>
          if ((prefix == LeapMonth) || ((prefix == LaterMonth) && !specialFlag))
            monthIndex -= 1

          if (monthIndex > 12)
            monthIndex -= 12
          
          result = Month(prefix + Numbers(monthIndex) + "月", word) :: result
          prefix = ""
          specialFlag = false
          monthIndex += 1
      }
    }
    val resultArray = result.reverse.toArray

    // Calcualte month length. Note that the length of the last month
    // will be calcualted in setMonthLength().
    for (i <- 0 until resultArray.length - 1) {
      resultArray(i).length =
        sexagenaryDiff(resultArray(i).sexagenary, resultArray(i + 1).sexagenary)
    }
    resultArray
  }

  // Map value to order concurrent dynastiest. The lower the order,
  // the higher the priority, i.e. appears first in
  // ChineseCalendar::fromDate().
  private val DynastyOrderMap = Map(
    "魏" -> 1, "蜀" -> 2, "吳" -> 3)

  def dynastyOrder(era: String) = {
    val c = era.substring(0, 1)
    if (DynastyOrderMap.contains(c)) DynastyOrderMap(c) else 0
  }

  /**
    * Represents one year in Chinese calendar. 
    * 
    * @param firstDay   the first day in Julian/Gregorian Calendar.
    * @param months     the 12 or 13 months in the year
    * @param sexagenary sexagenary of the year
    */
  private case class Year(var firstDay: JulianGregorianCalendar, months: Array[Month],
    var sexagenary: String)

  /** Return object Year given year, month, dayOfMonth, months. Assuming
    * 1st month is the start of each year. */
  private def y(year: Int, month: Int, dayOfMonth: Int, monthStr: String) =
    Year(date(year, month, dayOfMonth), months(monthStr), "")

  /** Return object Year given year, month, dayOfMonth, months. Assuming
    * 10th month is the start of each year. */
  private def z(year: Int, month: Int, dayOfMonth: Int, monthStr: String) =
    Year(date(year, month, dayOfMonth), months(monthStr, 10), "")  

  def date(year: Int, month: Int, dayOfMonth: Int) =
    new JulianGregorianCalendar(year, month, dayOfMonth)


  // Create a map from the array, with each value mapped to its index.
  def toMap(a: Array[String]) = {
    var map = new mutable.HashMap[String, Int]()
    for (i <- 0 to a.length - 1) {
      map(a(i)) = i
    }
    map
  }

  /** Return the array of era names. */
  def eraNames() = {
    eraArray.map(_._1).filter(_ != "").distinct
  }

  // Check sanity of year tables.
  private def checkYearTable(table: Array[Year]): Boolean = {
    // Calculate the first day of next year based on the first day of
    // current year and the sum of the sexagenary difference of the
    // months.
    var calculatedFirstDay = table(0).firstDay
    // The following value is selected to make the code also work for
    // the first year in the table.
    var prevSexagenary = table(0).months(0).sexagenary

    for (year <- table) {
      val Year(firstDay, months, _) = year
      if (months.length > 6) { // Ignore placeholders.
        calculatedFirstDay = calculatedFirstDay.plusDays(
          sexagenaryDiff(prevSexagenary, months(0).sexagenary))
 
        if (firstDay != calculatedFirstDay) {
          println("!!!!!!!!! " + firstDay + " CAL " + calculatedFirstDay + " !!!!!!!!!")
          return false
        }

        val (dayDiff, sexagenary) = daysFromNewYear(months.last.month, months)
        calculatedFirstDay = calculatedFirstDay.plusDays(dayDiff)
        prevSexagenary = sexagenary
      } else {
        return true
      }
    }

    true
  }


  // Calculate month length for the last month in each year.
  // Note that for the last month of the last year in a table, we need to call
  // setMonthLengthLastYear().
  private def setMonthLength(table: Array[Year]) {
    for (i <- 0 until table.length - 1) {
      val months = table(i).months
      val lastMonth = months(months.length - 1)
      val monthLength =
        sexagenaryDiff(lastMonth.sexagenary, table(i + 1).months(0).sexagenary)
      if ((lastMonth.length > 0) && (lastMonth.length != monthLength)) {
        // Since some year record is shared by different tables, check
        // whether there is collision. If yes, then corresponding data
        // will be copied to different tables instead of beinig shared.
        throw new RuntimeException("Sharing should be replaced by copying: index "
          + i + " months: " + months.mkString(":"))
      }
      lastMonth.length = monthLength
    }
  }

  // Set the month length of the last month of the last year in a
  // table, with the length from manual calculation.
  private def setMonthLengthLastYear(table: Array[Year], length: Int) {
    val lastYear = table(table.length - 1)
    val lastMonth = lastYear.months(lastYear.months.length - 1)
    // No more check as in setMonthLength since manual check is done.
    lastMonth.length = length
  }

  implicit class RichInt(val value: Int) extends AnyVal {
    def downto (n: Int) = value to n by -1
    def downtil (n: Int) = value until n by -1
  }

  // Set the date of the first day of each year of BCE years.
  private def setDateFirstDayBCE() {
    var firstDay = CEYears(0).firstDay
    var sexagenary = CEYears(0).months(0).sexagenary
    for (i <- BCEYears.length - 1 downto 0) {
      val year = BCEYears(i)
      var daysDiff = 0
      for (month <- year.months.reverse) {
        daysDiff += sexagenaryDiff(month.sexagenary, sexagenary)
        sexagenary = month.sexagenary
      }
      firstDay = firstDay.plusDays(-daysDiff)
      year.firstDay = firstDay
    }
  }

  /** Information for era segment, which is defined as a consecutive duration 
    * of one era.
    * @param era era name
    * @param startChinese the first day of the era segment in Chinese calendar
    * @param end the last day of the era segment 
    * @param prev the previous era.
    * @param next the next era.
    */
  case class EraSegment(era: String, startChinese: ChineseCalendar,
    var end: JulianGregorianCalendar,
    prev: String, next: String) {
    val start = toDate(startChinese, false)

    /** Equals method. */
    override def equals(other: Any): Boolean = other match {
      case that: EraSegment => era == that.era &&
        startChinese == that.startChinese && end == that.end &&
        prev == that.prev && next == that.next
      case _ => false
    }

    /** Returns true if the era segment contains the `date`. */
    def contains(date: JulianGregorianCalendar): Boolean =
      (start <= date) && (date <= end)

    /** Returns true if the era segment contains the `chineseDate`. */
    def contains(chineseDate: ChineseCalendar): Boolean = 
      contains(toDate(chineseDate, false)) && (era == chineseDate.era)

    /** Retruns true if this segments intersects``that` segment`. */
    def intersects(that: EraSegment) =
      contains(that.start) || contains(that.end)
  }

  /** Returns true if the `date` belongs to the duration(s) of an era. 
    * Note that an era could contain multiple durations. */
  def dateInRange(date: JulianGregorianCalendar, era: String) = 
    eraDurationMap(era).exists(_.contains(date))

  /** Return the list of segments containing `date`. */
  def containingSegments(date: JulianGregorianCalendar): List[EraSegment] = {
    /* start, end are indices to eraSegmentArray, denoting a subarray
     * of [start, end). */
    def rec(start: Int, end: Int, acc: List[EraSegment]): List[EraSegment] = {
      if (start >= end) { // Zero element
        acc
      } else {
        val mid = (start + end) / 2
        val eraSegment = eraSegmentArray(mid)
        if (date < eraSegment.start) {
          rec(start, mid, acc)
        } else {
          val result = rec(start, mid, Nil) ::: rec(mid + 1, end, acc)
          if (date <= eraSegment.end) eraSegment :: result
          else result
        }
      }
    }

    def binarySearch(start: Int, end: Int): Int = {
      if (start >= end) {
        throw new RuntimeException("containingSegments|binarySearch(): null search result.")
      }

      // We don't care overflow here as the size of eraPartitionArray is very small.
      val mid = (start + end) / 2

      // Three way comparison.
      val midSegment = eraSegmentArray(eraPartitionArray(mid))
      if (date < midSegment.start) {
        binarySearch(start, mid)
      } else {
        // Handle the case that eraSegment is the last one.  The
        // reason we do such special handling here is that we don't
        // want to book keep th end time of each duration.
        if (mid == eraPartitionArray.length - 1) {
          return mid
        }

        val nextSegment = eraSegmentArray(eraPartitionArray(mid + 1))
        if (date >= nextSegment.start) {
          binarySearch(mid + 1, end)
        } else {
          mid
        }
      }
    }

    val partitionIndex = binarySearch(0, eraPartitionArray.length)
    val endIndex =
      if (partitionIndex == eraPartitionArray.length - 1) eraSegmentArray.length
      else eraPartitionArray(partitionIndex + 1)
    rec(eraPartitionArray(partitionIndex), endIndex, Nil)
  }

  /** Return the segment containing `chineseDate`. */
  def containingSegment(chineseDate: ChineseCalendar): Option[EraSegment] = {
    // By history, there is at most one segment as the result.
    val atMostOne = containingSegments(toDate(chineseDate, false)).filter(_.era == chineseDate.era)
    atMostOne match {
      case Nil => None
      case List(x) => Some(x)
      case _ => throw new IllegalArgumentException("containingSegment(): illegal input " + chineseDate)       
    }
  }

  // Calculate the sexagenary in each year.
  // @param startSexagenary the sexagenary of the first year in the table.
  private def setSexagenary(startSexagenary: String, table: Array[Year]) {
    assert((table(0).sexagenary == "") || table(0).sexagenary == startSexagenary)

    for (i <- 0 until table.length) {
      table(i).sexagenary = sexagenaryAdd(startSexagenary, i)
    }
  }

  /**
    * To optimize performance of containingSegments(), segment
    * eraSegmentArray into partitions where overlaping era segments
    * are contained in one partition.
    * 
    * This is a helper function which will be called by
    * processEraArray() to actually finish the partition work, which
    * contains a list of start indices for binary search in
    * containingSegments().
    */
  private def partitionEraSegments(initialIndex: Int, endIndex: Int): Option[Int] = {
    var i = initialIndex + 1
    while (i < endIndex) {
      // It is more likely to overlap with a nearby era segment, so test them first.
      val prevIndices = (initialIndex until i).reverse
      if (prevIndices.exists(eraSegmentArray(_) intersects eraSegmentArray(i))) {
        i += 1
      } else {
        return Some(i)
      }
    }
    None
  }

  /* Post process eraArray to generate eraSegmentArray. */
  private def processEraArray() {
    for (i <- 0 until eraArray.length) {
      val (eraName, start, end, prev, next, _) = eraArray(i)

      val eraStartSuffix =
        if (start == "") "元年"
        else if (start.contains("年")) start
        else "元年" + start
      val startChinese = parseDate(eraName + eraStartSuffix)

      val eraEndSuffix =
        if (end.contains("年")) {
          if (end.endsWith("月")) end + "晦"
          else end
        }
        else if (end != "") {
          if (end.endsWith("月")) "元年" + end + "晦"
          else "元年" + end          
        }
        else ""

      val endDate =
        // We don't handle default case here as it is simpler to calculate it later.
        if (end == "") date(0, 1, 1)
        else toDate(eraName + eraEndSuffix, false)

      val prevNorm =
        if ((prev == "") && (i > 0)) eraArray(i - 1)._1
        else prev

      val nextNorm = 
        if ((next == "") && (i < eraArray.length - 1))  eraArray(i + 1)._1
        else next

      eraSegmentArray(i) = EraSegment(eraName, startChinese, endDate, prevNorm, nextNorm)
    }

    // 2nd pass, to calculate the end date for defualt case.
    for (i <- 0 until eraSegmentArray.length) {
      if (eraArray(i)._3 == "") { // Default case for end.
        assert(i != eraSegmentArray.length - 1)
        if (eraArray(i)._5 == "") { // The next entry is the next era segment in time.
          eraSegmentArray(i).end = eraSegmentArray(i + 1).start.plusDays(-1)
        } else { // The next entry is NOT the next era segment in time.
          val Some(nextEraSegment) = eraSegmentArray.find(_.era == eraArray(i)._5)
          eraSegmentArray(i).end = nextEraSegment.start.plusDays(-1)
        }
      }
    }

    eraSegmentArray = eraSegmentArray.sortBy(_.start)

    // 3rd pass, to generate eraDurationMap.
    for (eraSegment <- eraSegmentArray) {
      val era = eraSegment.era
      if (eraDurationMap.contains(era)) {
        eraDurationMap(era) = eraSegment :: eraDurationMap(eraSegment.era)
      } else {
        eraDurationMap(era) = List(eraSegment)        
      }
    }

    // 4th pass, to generate eraParitionArray.
    var initialIndex = 0
    val endIndex = eraSegmentArray.length
    var eraPartitionList = List(0)
    var continue = true
    do {
      partitionEraSegments(initialIndex, endIndex) match {
        case Some(index) =>
          eraPartitionList = index :: eraPartitionList
          initialIndex = index
        case None =>
          continue = false
      }
    } while (continue)
    eraPartitionArray = eraPartitionList.reverse.toArray
  }

  def checkEveryDay(): Boolean = {
    var day = FirstDay
    while (day < LastDay) {
      val chineseDates = fromDate(day)
      for (chineseDate <- chineseDates) {
        if (toDate(chineseDate, true) != day) {
          return false
        }
      }
      day = day.plusDays(1)
    }

    true
  }

  // Regresssion test to ensure the data tables are correct. Made
  // public so this can be called as regression test.
  def sanityCheck: Boolean = {
    checkYearTable(BCEYears) &&
    checkYearTable(CEYears) &&
    checkYearTable(ShuYears) &&
    checkYearTable(WuYears) &&
    checkYearTable(BeiWeiYears)
  }

  /** Returns true if `s` in form of Julian/Gregorian Calendar. */
  def jgQuery(s: String) = {
    Character.isDigit(s(0)) || s.startsWith("公元前")
  }

  private val predictionMap = new mutable.HashMap[String, Array[String]]()
  predictionMap("") = Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "公")
  predictionMap("公") = Array("元前")
  predictionMap("公元") = Array("前")
  predictionMap("公元前") = Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "0")

  def buildJGPrediction(lastYear: Int, prefix: String) {
    val lastYearLength = ("" + lastYear).length
    for (i <- 1 to lastYear) {
      val year = "" + i
      var list: List[String] = Nil
      if (year.length < lastYearLength) {
        for (j <- 1 to 9) {
          if (i * 10 + j <= lastYear) {
            list = ("" + j) :: list
          }
        }

        // We want to place 0 at the end of candicates.          
        if (list.length > 0) {
          list = ("" + 0) :: list
        }
      }
      predictionMap(prefix + year) = ("年" :: list).reverse.toArray
    }    
  }

  def buildPrediction() {
    buildJGPrediction(LastDay.year, "")
    buildJGPrediction(1 - FirstDay.year, "公元前")
  }

  // Regex used for nextCharacter().
  private val JGYearD = new Regex("^(公元前)?(\\d+)年(\\d{0,2})")
  private val JGMonthD = new Regex("^(公元前)?(\\d+)年(\\d{1,2})月(\\d{0,2})")  

  /**
    * Return an array of characters, which is the prediction result
    * based on input `query`, suitable as input for calendar
    * conversion in app `calendarlookup`.
    * 
    * Both input and output strings are in traiditional Chinese.
    * 
    * See calendarlookup/SearchActivity:checkInput() for how to handle
    * different output from `nextCharacter`.
    */
  def nextCharacter(query: String): Array[String] = {
    predictionMap.get(query) match {
      case None =>
        if (query.endsWith("日"))
          return Array("")

        if (jgQuery(query)) {
          val firstDay = JulianGregorianCalendar.fromStringFrag(query)

          if (JGMonthD.findFirstIn(query) != None) {
            // TODO: perform optimization to only show the
            // available months for the first year and last year, or
            // Julian / Gregorian cutoff.

            val JGMonthD(bce, year, month, day) = query

            if ((day == null) || (day.length == 0)) {
              return Array("1", "2", "3", "4", "5", "6", "7", "8", "9")
            }

            if (day.length == 2) {
              return Array("日")
            }

            val m = Integer.parseInt(month)
            val d = Integer.parseInt(day)
            val monthFirstDay = date(firstDay.year, m, 1)

            d match {
              case 1 =>
                return Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "日")

              case 2 =>
                if (monthFirstDay.isLeapYear()) {
                  return Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "日")
                } else {
                  return Array("1", "2", "3", "4", "5", "6", "7", "8", "0", "日")                  
                }

              case 3 =>
                monthFirstDay.monthDays() match {
                  case 30 =>
                    return Array("0", "日")
                  case 31 =>
                    return Array("1", "0", "日")                    
                  case _ =>
                    return Array("日")                                        
                }

              case _ =>
                return Array("日")
            }
          }

          if (JGYearD.findFirstIn(query) != None) {
            // TODO: perform optimization to only show the
            // available months for the first year and last year.
            val JGYearD(bce, year, month) = query
            if ((month == null) || (month.length == 0)) {
              return Array("1", "2", "3", "4", "5", "6", "7", "8", "9")
            }

            if (month.length == 2) {
              return Array("月")
            }

            if (Integer.parseInt(month) == 1) {
              return Array("1", "2", "0", "月")
            } else {
              return Array("月")
            }
          }          
        } else {
        }

        null

      case Some(a) => a
    }
  }
  
  // Information from
  //   * 三千五百年历日天象 (张培瑜 著)  
  //   * 中国史历日和中西历日对照表 (方诗铭，方小芬 著)
  // It's possible that different calendars are used in the same time,
  // but they may share the same calendar for some time.
  // Example: 魏文帝黃初 and 吳大帝黃武

  // The date of the first day of the year is calculated instead of
  // specified.
  private val BCEYears = Array(
    // z(-840, 2, 28, ""),
    // z(-839, 2, 28, ""),            
    // z(-838, 2, 28, ""),        
    // z(-837, 2, 28, ""),        
    // z(-836, 2, 28, ""),        
    // z(-835, 2, 28, ""),        
    // z(-834, 2, 28, ""),        
    // z(-833, 2, 28, ""),        
    // z(-832, 2, 28, ""),        
    // z(-831, 2, 28, ""),    
    // z(-830, 2, 28, ""),
    // z(-829, 2, 28, ""),            
    // z(-828, 2, 28, ""),        
    // z(-827, 2, 28, ""),        
    // z(-826, 2, 28, ""),        
    // z(-825, 2, 28, ""),        
    // z(-824, 2, 28, ""),        
    // z(-823, 2, 28, ""),        
    // z(-822, 2, 28, ""),        
    // z(-821, 2, 28, ""),    
    // z(-820, 2, 28, ""),     
    // z(-819, 2, 28, ""),            
    // z(-818, 2, 28, ""),        
    // z(-817, 2, 28, ""),        
    // z(-816, 2, 28, ""),        
    // z(-815, 2, 28, ""),        
    // z(-814, 2, 28, ""),        
    // z(-813, 2, 28, ""),        
    // z(-812, 2, 28, ""),        
    // z(-811, 2, 28, ""),    
    // z(-810, 2, 28, ""),      
    // z(-809, 2, 28, ""),            
    // z(-808, 2, 28, ""),        
    // z(-807, 2, 28, ""),        
    // z(-806, 2, 28, ""),        
    // z(-805, 2, 28, ""),        
    // z(-804, 2, 28, ""),        
    // z(-803, 2, 28, ""),        
    // z(-802, 2, 28, ""),        
    // z(-801, 2, 28, ""),    
    // z(-800, 2, 28, ""),
    // z(-799, 2, 28, ""),            
    // z(-798, 2, 28, ""),        
    // z(-797, 2, 28, ""),        
    // z(-796, 2, 28, ""),        
    // z(-795, 2, 28, ""),        
    // z(-794, 2, 28, ""),        
    // z(-793, 2, 28, ""),        
    // z(-792, 2, 28, ""),        
    // z(-791, 2, 28, ""),    
    // z(-790, 2, 28, ""),
    // z(-789, 2, 28, ""),            
    // z(-788, 2, 28, ""),        
    // z(-787, 2, 28, ""),        
    // z(-786, 2, 28, ""),        
    // z(-785, 2, 28, ""),        
    // z(-784, 2, 28, ""),        
    // z(-783, 2, 28, ""),        
    // z(-782, 2, 28, ""),        
    // z(-781, 2, 28, ""),    
    // z(-780, 2, 28, ""),
    // z(-779, 2, 28, ""),            
    // z(-778, 2, 28, ""),        
    // z(-777, 2, 28, ""),        
    // z(-776, 2, 28, ""),        
    // z(-775, 2, 28, ""),        
    // z(-774, 2, 28, ""),        
    // z(-773, 2, 28, ""),        
    // z(-772, 2, 28, ""),        
    // z(-771, 2, 28, ""),    
    // z(-770, 2, 28, ""),
    // z(-769, 2, 28, ""),            
    // z(-768, 2, 28, ""),        
    // z(-767, 2, 28, ""),        
    // z(-766, 2, 28, ""),        
    // z(-765, 2, 28, ""),        
    // z(-764, 2, 28, ""),        
    // z(-763, 2, 28, ""),        
    // z(-762, 2, 28, ""),        
    // z(-761, 2, 28, ""),    
    // z(-760, 2, 28, ""),
    // z(-759, 2, 28, ""),            
    // z(-758, 2, 28, ""),        
    // z(-757, 2, 28, ""),        
    // z(-756, 2, 28, ""),        
    // z(-755, 2, 28, ""),        
    // z(-754, 2, 28, ""),        
    // z(-753, 2, 28, ""),        
    // z(-752, 2, 28, ""),        
    // z(-751, 2, 28, ""),    
    // z(-750, 2, 28, ""),
    // z(-749, 2, 28, ""),            
    // z(-748, 2, 28, ""),        
    // z(-747, 2, 28, ""),        
    // z(-746, 2, 28, ""),        
    // z(-745, 2, 28, ""),        
    // z(-744, 2, 28, ""),        
    // z(-743, 2, 28, ""),        
    // z(-742, 2, 28, ""),        
    // z(-741, 2, 28, ""),    
    // z(-740, 2, 28, ""),
    // z(-739, 2, 28, ""),            
    // z(-738, 2, 28, ""),        
    // z(-737, 2, 28, ""),        
    // z(-736, 2, 28, ""),        
    // z(-735, 2, 28, ""),        
    // z(-734, 2, 28, ""),        
    // z(-733, 2, 28, ""),        
    // z(-732, 2, 28, ""),        
    // z(-731, 2, 28, ""),    
    // z(-730, 2, 28, ""),
    // z(-729, 2, 28, ""),            
    // z(-728, 2, 28, ""),        
    // z(-727, 2, 28, ""),        
    // z(-726, 2, 28, ""),        
    // z(-725, 2, 28, ""),        
    // z(-724, 2, 28, ""),        
    // z(-723, 2, 28, ""),        
    // z(-722, 2, 28, ""),        
    // z(-721, 2, 28, ""),    
    // z(-720, 2, 28, ""),     
    // z(-719, 2, 28, ""),            
    // z(-718, 2, 28, ""),        
    // z(-717, 2, 28, ""),        
    // z(-716, 2, 28, ""),        
    // z(-715, 2, 28, ""),        
    // z(-714, 2, 28, ""),        
    // z(-713, 2, 28, ""),        
    // z(-712, 2, 28, ""),        
    // z(-711, 2, 28, ""),    
    // z(-710, 2, 28, ""),      
    // z(-709, 2, 28, ""),            
    // z(-708, 2, 28, ""),        
    // z(-707, 2, 28, ""),        
    // z(-706, 2, 28, ""),        
    // z(-705, 2, 28, ""),        
    // z(-704, 2, 28, ""),        
    // z(-703, 2, 28, ""),        
    // z(-702, 2, 28, ""),        
    // z(-701, 2, 28, ""),
    // z(-700, 2, 28, ""),
    // z(-699, 2, 28, ""),            
    // z(-698, 2, 28, ""),        
    // z(-697, 2, 28, ""),        
    // z(-696, 2, 28, ""),        
    // z(-695, 2, 28, ""),        
    // z(-694, 2, 28, ""),        
    // z(-693, 2, 28, ""),        
    // z(-692, 2, 28, ""),        
    // z(-691, 2, 28, ""),    
    // z(-690, 2, 28, ""),
    // z(-689, 2, 28, ""),            
    // z(-688, 2, 28, ""),        
    // z(-687, 2, 28, ""),        
    // z(-686, 2, 28, ""),        
    // z(-685, 2, 28, ""),        
    // z(-684, 2, 28, ""),        
    // z(-683, 2, 28, ""),        
    // z(-682, 2, 28, ""),        
    // z(-681, 2, 28, ""),    
    // z(-680, 2, 28, ""),
    // z(-679, 2, 28, ""),            
    // z(-678, 2, 28, ""),        
    // z(-677, 2, 28, ""),        
    // z(-676, 2, 28, ""),        
    // z(-675, 2, 28, ""),        
    // z(-674, 2, 28, ""),        
    // z(-673, 2, 28, ""),        
    // z(-672, 2, 28, ""),        
    // z(-671, 2, 28, ""),    
    // z(-670, 2, 28, ""),
    // z(-669, 2, 28, ""),            
    // z(-668, 2, 28, ""),        
    // z(-667, 2, 28, ""),        
    // z(-666, 2, 28, ""),        
    // z(-665, 2, 28, ""),        
    // z(-664, 2, 28, ""),        
    // z(-663, 2, 28, ""),        
    // z(-662, 2, 28, ""),        
    // z(-661, 2, 28, ""),    
    // z(-660, 2, 28, ""),
    // z(-659, 2, 28, ""),            
    // z(-658, 2, 28, ""),        
    // z(-657, 2, 28, ""),        
    // z(-656, 2, 28, ""),        
    // z(-655, 2, 28, ""),        
    // z(-654, 2, 28, ""),        
    // z(-653, 2, 28, ""),        
    // z(-652, 2, 28, ""),        
    // z(-651, 2, 28, ""),    
    // z(-650, 2, 28, ""),
    // z(-649, 2, 28, ""),            
    // z(-648, 2, 28, ""),        
    // z(-647, 2, 28, ""),        
    // z(-646, 2, 28, ""),        
    // z(-645, 2, 28, ""),        
    // z(-644, 2, 28, ""),        
    // z(-643, 2, 28, ""),        
    // z(-642, 2, 28, ""),        
    // z(-641, 2, 28, ""),    
    // z(-640, 2, 28, ""),
    // z(-639, 2, 28, ""),            
    // z(-638, 2, 28, ""),        
    // z(-637, 2, 28, ""),        
    // z(-636, 2, 28, ""),        
    // z(-635, 2, 28, ""),        
    // z(-634, 2, 28, ""),        
    // z(-633, 2, 28, ""),        
    // z(-632, 2, 28, ""),        
    // z(-631, 2, 28, ""),    
    // z(-630, 2, 28, ""),
    // z(-629, 2, 28, ""),            
    // z(-628, 2, 28, ""),        
    // z(-627, 2, 28, ""),        
    // z(-626, 2, 28, ""),        
    // z(-625, 2, 28, ""),        
    // z(-624, 2, 28, ""),        
    // z(-623, 2, 28, ""),        
    // z(-622, 2, 28, ""),        
    // z(-621, 2, 28, ""),    
    // z(-620, 2, 28, ""),     
    // z(-619, 2, 28, ""),            
    // z(-618, 2, 28, ""),        
    // z(-617, 2, 28, ""),        
    // z(-616, 2, 28, ""),        
    // z(-615, 2, 28, ""),        
    // z(-614, 2, 28, ""),        
    // z(-613, 2, 28, ""),        
    // z(-612, 2, 28, ""),        
    // z(-611, 2, 28, ""),    
    // z(-610, 2, 28, ""),      
    // z(-609, 2, 28, ""),            
    // z(-608, 2, 28, ""),        
    // z(-607, 2, 28, ""),        
    // z(-606, 2, 28, ""),        
    // z(-605, 2, 28, ""),        
    // z(-604, 2, 28, ""),        
    // z(-603, 2, 28, ""),        
    // z(-602, 2, 28, ""),        
    // z(-601, 2, 28, ""),    
    // z(-600, 2, 28, ""),      
    // z(-599, 2, 28, ""),            
    // z(-598, 2, 28, ""),        
    // z(-597, 2, 28, ""),        
    // z(-596, 2, 28, ""),        
    // z(-595, 2, 28, ""),        
    // z(-594, 2, 28, ""),        
    // z(-593, 2, 28, ""),        
    // z(-592, 2, 28, ""),        
    // z(-591, 2, 28, ""),    
    // z(-590, 2, 28, ""),
    // z(-589, 2, 28, ""),            
    // z(-588, 2, 28, ""),        
    // z(-587, 2, 28, ""),        
    // z(-586, 2, 28, ""),        
    // z(-585, 2, 28, ""),        
    // z(-584, 2, 28, ""),        
    // z(-583, 2, 28, ""),        
    // z(-582, 2, 28, ""),        
    // z(-581, 2, 28, ""),    
    // z(-580, 2, 28, ""),
    // z(-579, 2, 28, ""),            
    // z(-578, 2, 28, ""),        
    // z(-577, 2, 28, ""),        
    // z(-576, 2, 28, ""),        
    // z(-575, 2, 28, ""),        
    // z(-574, 2, 28, ""),        
    // z(-573, 2, 28, ""),        
    // z(-572, 2, 28, ""),        
    // z(-571, 2, 28, ""),    
    // z(-570, 2, 28, ""),
    // z(-569, 2, 28, ""),            
    // z(-568, 2, 28, ""),        
    // z(-567, 2, 28, ""),        
    // z(-566, 2, 28, ""),        
    // z(-565, 2, 28, ""),        
    // z(-564, 2, 28, ""),        
    // z(-563, 2, 28, ""),        
    // z(-562, 2, 28, ""),        
    // z(-561, 2, 28, ""),    
    // z(-560, 2, 28, ""),
    // z(-559, 2, 28, ""),            
    // z(-558, 2, 28, ""),        
    // z(-557, 2, 28, ""),        
    // z(-556, 2, 28, ""),        
    // z(-555, 2, 28, ""),        
    // z(-554, 2, 28, ""),        
    // z(-553, 2, 28, ""),        
    // z(-552, 2, 28, ""),        
    // z(-551, 2, 28, ""),    
    // z(-550, 2, 28, ""),
    // z(-549, 2, 28, ""),            
    // z(-548, 2, 28, ""),        
    // z(-547, 2, 28, ""),        
    // z(-546, 2, 28, ""),        
    // z(-545, 2, 28, ""),        
    // z(-544, 2, 28, ""),        
    // z(-543, 2, 28, ""),        
    // z(-542, 2, 28, ""),        
    // z(-541, 2, 28, ""),    
    // z(-540, 2, 28, ""),
    // z(-539, 2, 28, ""),            
    // z(-538, 2, 28, ""),        
    // z(-537, 2, 28, ""),        
    // z(-536, 2, 28, ""),        
    // z(-535, 2, 28, ""),        
    // z(-534, 2, 28, ""),        
    // z(-533, 2, 28, ""),        
    // z(-532, 2, 28, ""),        
    // z(-531, 2, 28, ""),    
    // z(-530, 2, 28, ""),
    // z(-529, 2, 28, ""),            
    // z(-528, 2, 28, ""),        
    // z(-527, 2, 28, ""),        
    // z(-526, 2, 28, ""),        
    // z(-525, 2, 28, ""),        
    // z(-524, 2, 28, ""),        
    // z(-523, 2, 28, ""),        
    // z(-522, 2, 28, ""),        
    // z(-521, 2, 28, ""),    
    // z(-520, 2, 28, ""),     
    // z(-519, 2, 28, ""),            
    // z(-518, 2, 28, ""),        
    // z(-517, 2, 28, ""),        
    // z(-516, 2, 28, ""),        
    // z(-515, 2, 28, ""),        
    // z(-514, 2, 28, ""),        
    // z(-513, 2, 28, ""),        
    // z(-512, 2, 28, ""),        
    // z(-511, 2, 28, ""),    
    // z(-510, 2, 28, ""),      
    // z(-509, 2, 28, ""),            
    // z(-508, 2, 28, ""),        
    // z(-507, 2, 28, ""),        
    // z(-506, 2, 28, ""),        
    // z(-505, 2, 28, ""),        
    // z(-504, 2, 28, ""),        
    // z(-503, 2, 28, ""),        
    // z(-502, 2, 28, ""),        
    // z(-501, 2, 28, ""),    
    // z(-500, 2, 28, ""),
    // z(-499, 2, 28, ""),            
    // z(-498, 2, 28, ""),        
    // z(-497, 2, 28, ""),        
    // z(-496, 2, 28, ""),        
    // z(-495, 2, 28, ""),        
    // z(-494, 2, 28, ""),        
    // z(-493, 2, 28, ""),        
    // z(-492, 2, 28, ""),        
    // z(-491, 2, 28, ""),    
    // z(-490, 2, 28, ""),
    // z(-489, 2, 28, ""),            
    // z(-488, 2, 28, ""),        
    // z(-487, 2, 28, ""),        
    // z(-486, 2, 28, ""),        
    // z(-485, 2, 28, ""),        
    // z(-484, 2, 28, ""),        
    // z(-483, 2, 28, ""),        
    // z(-482, 2, 28, ""),        
    // z(-481, 2, 28, ""),    
    // z(-480, 2, 28, ""),
    // z(-479, 2, 28, ""),            
    // z(-478, 2, 28, ""),        
    // z(-477, 2, 28, ""),        
    // z(-476, 2, 28, ""),        
    // z(-475, 2, 28, ""),        
    // z(-474, 2, 28, ""),        
    // z(-473, 2, 28, ""),        
    // z(-472, 2, 28, ""),        
    // z(-471, 2, 28, ""),    
    // z(-470, 2, 28, ""),
    // z(-469, 2, 28, ""),            
    // z(-468, 2, 28, ""),        
    // z(-467, 2, 28, ""),        
    // z(-466, 2, 28, ""),        
    // z(-465, 2, 28, ""),        
    // z(-464, 2, 28, ""),        
    // z(-463, 2, 28, ""),        
    // z(-462, 2, 28, ""),        
    // z(-461, 2, 28, ""),    
    // z(-460, 2, 28, ""),
    // z(-459, 2, 28, ""),            
    // z(-458, 2, 28, ""),        
    // z(-457, 2, 28, ""),        
    // z(-456, 2, 28, ""),        
    // z(-455, 2, 28, ""),        
    // z(-454, 2, 28, ""),        
    // z(-453, 2, 28, ""),        
    // z(-452, 2, 28, ""),        
    // z(-451, 2, 28, ""),    
    // z(-450, 2, 28, ""),
    // z(-449, 2, 28, ""),            
    // z(-448, 2, 28, ""),        
    // z(-447, 2, 28, ""),        
    // z(-446, 2, 28, ""),        
    // z(-445, 2, 28, ""),        
    // z(-444, 2, 28, ""),        
    // z(-443, 2, 28, ""),        
    // z(-442, 2, 28, ""),        
    // z(-441, 2, 28, ""),    
    // z(-440, 2, 28, ""),
    // z(-439, 2, 28, ""),            
    // z(-438, 2, 28, ""),        
    // z(-437, 2, 28, ""),        
    // z(-436, 2, 28, ""),        
    // z(-435, 2, 28, ""),        
    // z(-434, 2, 28, ""),        
    // z(-433, 2, 28, ""),        
    // z(-432, 2, 28, ""),        
    // z(-431, 2, 28, ""),    
    // z(-430, 2, 28, ""),
    // z(-429, 2, 28, ""),            
    // z(-428, 2, 28, ""),        
    // z(-427, 2, 28, ""),        
    // z(-426, 2, 28, ""),        
    // z(-425, 2, 28, ""),        
    // z(-424, 2, 28, ""),        
    // z(-423, 2, 28, ""),        
    // z(-422, 2, 28, ""),        
    // z(-421, 2, 28, ""),    
    // z(-420, 2, 28, ""),     
    // z(-419, 2, 28, ""),            
    // z(-418, 2, 28, ""),        
    // z(-417, 2, 28, ""),        
    // z(-416, 2, 28, ""),        
    // z(-415, 2, 28, ""),        
    // z(-414, 2, 28, ""),        
    // z(-413, 2, 28, ""),        
    // z(-412, 2, 28, ""),        
    // z(-411, 2, 28, ""),    
    // z(-410, 2, 28, ""),      
    // z(-409, 2, 28, ""),            
    // z(-408, 2, 28, ""),        
    // z(-407, 2, 28, ""),        
    // z(-406, 2, 28, ""),        
    // z(-405, 2, 28, ""),        
    // z(-404, 2, 28, ""),        
    // z(-403, 2, 28, ""),        
    // z(-402, 2, 28, ""),        
    // z(-401, 2, 28, ""),    
    // z(-400, 2, 28, ""),
    // z(-399, 2, 28, ""),            
    // z(-398, 2, 28, ""),        
    // z(-397, 2, 28, ""),        
    // z(-396, 2, 28, ""),        
    // z(-395, 2, 28, ""),        
    // z(-394, 2, 28, ""),        
    // z(-393, 2, 28, ""),        
    // z(-392, 2, 28, ""),        
    // z(-391, 2, 28, ""),    
    // z(-390, 2, 28, ""),
    // z(-389, 2, 28, ""),            
    // z(-388, 2, 28, ""),        
    // z(-387, 2, 28, ""),        
    // z(-386, 2, 28, ""),        
    // z(-385, 2, 28, ""),        
    // z(-384, 2, 28, ""),        
    // z(-383, 2, 28, ""),        
    // z(-382, 2, 28, ""),        
    // z(-381, 2, 28, ""),    
    // z(-380, 2, 28, ""),
    // z(-379, 2, 28, ""),            
    // z(-378, 2, 28, ""),        
    // z(-377, 2, 28, ""),        
    // z(-376, 2, 28, ""),        
    // z(-375, 2, 28, ""),        
    // z(-374, 2, 28, ""),        
    // z(-373, 2, 28, ""),        
    // z(-372, 2, 28, ""),        
    // z(-371, 2, 28, ""),    
    // z(-370, 2, 28, ""),
    // z(-369, 2, 28, ""),            
    // z(-368, 2, 28, ""),        
    // z(-367, 2, 28, ""),        
    // z(-366, 2, 28, ""),        
    // z(-365, 2, 28, ""),        
    // z(-364, 2, 28, ""),        
    // z(-363, 2, 28, ""),        
    // z(-362, 2, 28, ""),        
    // z(-361, 2, 28, ""),    
    // z(-360, 2, 28, ""),
    // z(-359, 2, 28, ""),            
    // z(-358, 2, 28, ""),        
    // z(-357, 2, 28, ""),        
    // z(-356, 2, 28, ""),        
    // z(-355, 2, 28, ""),        
    // z(-354, 2, 28, ""),        
    // z(-353, 2, 28, ""),        
    // z(-352, 2, 28, ""),        
    // z(-351, 2, 28, ""),    
    // z(-350, 2, 28, ""),
    // z(-349, 2, 28, ""),            
    // z(-348, 2, 28, ""),        
    // z(-347, 2, 28, ""),        
    // z(-346, 2, 28, ""),        
    // z(-345, 2, 28, ""),        
    // z(-344, 2, 28, ""),        
    // z(-343, 2, 28, ""),        
    // z(-342, 2, 28, ""),        
    // z(-341, 2, 28, ""),    
    // z(-340, 2, 28, ""),
    // z(-339, 2, 28, ""),            
    // z(-338, 2, 28, ""),        
    // z(-337, 2, 28, ""),        
    // z(-336, 2, 28, ""),        
    // z(-335, 2, 28, ""),        
    // z(-334, 2, 28, ""),        
    // z(-333, 2, 28, ""),        
    // z(-332, 2, 28, ""),        
    // z(-331, 2, 28, ""),    
    // z(-330, 2, 28, ""),
    // z(-329, 2, 28, ""),            
    // z(-328, 2, 28, ""),        
    // z(-327, 2, 28, ""),        
    // z(-326, 2, 28, ""),        
    // z(-325, 2, 28, ""),        
    // z(-324, 2, 28, ""),        
    // z(-323, 2, 28, ""),        
    // z(-322, 2, 28, ""),        
    // z(-321, 2, 28, ""),    
    // z(-320, 2, 28, ""),     
    // z(-319, 2, 28, ""),            
    // z(-318, 2, 28, ""),        
    // z(-317, 2, 28, ""),        
    // z(-316, 2, 28, ""),        
    // z(-315, 2, 28, ""),        
    // z(-314, 2, 28, ""),        
    // z(-313, 2, 28, ""),        
    // z(-312, 2, 28, ""),        
    // z(-311, 2, 28, ""),    
    // z(-310, 2, 28, ""),      
    // z(-309, 2, 28, ""),            
    // z(-308, 2, 28, ""),        
    // z(-307, 2, 28, ""),        
    // z(-306, 2, 28, ""),        
    // z(-305, 2, 28, ""),        
    // z(-304, 2, 28, ""),        
    // z(-303, 2, 28, ""),        
    // z(-302, 2, 28, ""),        
    // z(-301, 2, 28, ""),    
    // z(-300, 2, 28, ""),
    // z(-299, 2, 28, ""),            
    // z(-298, 2, 28, ""),        
    // z(-297, 2, 28, ""),        
    // z(-296, 2, 28, ""),        
    // z(-295, 2, 28, ""),        
    // z(-294, 2, 28, ""),        
    // z(-293, 2, 28, ""),        
    // z(-292, 2, 28, ""),        
    // z(-291, 2, 28, ""),    
    // z(-290, 2, 28, ""),
    // z(-289, 2, 28, ""),            
    // z(-288, 2, 28, ""),        
    // z(-287, 2, 28, ""),        
    // z(-286, 2, 28, ""),        
    // z(-285, 2, 28, ""),        
    // z(-284, 2, 28, ""),        
    // z(-283, 2, 28, ""),        
    // z(-282, 2, 28, ""),        
    // z(-281, 2, 28, ""),    
    // z(-280, 2, 28, ""),
    // z(-279, 2, 28, ""),            
    // z(-278, 2, 28, ""),        
    // z(-277, 2, 28, ""),        
    // z(-276, 2, 28, ""),        
    // z(-275, 2, 28, ""),        
    // z(-274, 2, 28, ""),        
    // z(-273, 2, 28, ""),        
    // z(-272, 2, 28, ""),        
    // z(-271, 2, 28, ""),    
    // z(-270, 2, 28, ""),
    // z(-269, 2, 28, ""),            
    // z(-268, 2, 28, ""),        
    // z(-267, 2, 28, ""),        
    // z(-266, 2, 28, ""),        
    // z(-265, 2, 28, ""),        
    // z(-264, 2, 28, ""),        
    // z(-263, 2, 28, ""),        
    // z(-262, 2, 28, ""),        
    // z(-261, 2, 28, ""),    
    // z(-260, 2, 28, ""),
    // z(-259, 2, 28, ""),            
    // z(-258, 2, 28, ""),        
    // z(-257, 2, 28, ""),        
    // z(-256, 2, 28, ""),        
    // z(-255, 2, 28, ""),        
    // z(-254, 2, 28, ""),        
    // z(-253, 2, 28, ""),        
    // z(-252, 2, 28, ""),        
    // z(-251, 2, 28, ""),    
    z(-250, 2, 28, "壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丁卯"),
    z(-249, 2, 28, "丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉"),            
    z(-248, 2, 28, "庚寅 庚申 己丑 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 後 乙酉"),        
    z(-247, 2, 28, "甲寅 甲申 癸丑 癸未 壬子 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯"),
    // According to 三千五百年历日天象 (张培瑜 著)  
    z(-246, 2, 28, "己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 甲戌 後 癸卯"),        
    z(-245, 2, 28, "癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉"),        
    z(-244, 2, 28, "丁卯 丙申 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰"),        
    z(-243, 2, 28, "辛酉 辛卯 庚申 庚寅 己未 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 後 丙辰"),        
    z(-242, 2, 28, "乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 辛巳 庚戌"),        
    z(-241, 2, 28, "庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰"),    
    z(-240, 2, 28, "甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 後 戊辰"),
    z(-239, 2, 28, "戊戌 丁卯 丁酉 丙寅 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥"),            
    z(-238, 2, 28, "壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 戊子 丁巳 後 丁亥"),        
    z(-237, 2, 28, "丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳"),        
    z(-236, 2, 28, "辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥"),        
    z(-235, 2, 28, "乙巳 甲戌 甲辰 癸酉 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 後 己亥"),        
    z(-234, 2, 28, "己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 丙申 乙丑 乙未 甲子 甲午"),        
    z(-233, 2, 28, "癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子"),        
    z(-232, 2, 28, "戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 後 壬子"),        
    z(-231, 2, 28, "辛巳 辛亥 庚辰 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午"),    
    z(-230, 2, 28, "丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 癸卯 壬申 壬寅 辛未 辛丑"),
    z(-229, 2, 28, "庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 後 乙丑"),            
    z(-228, 2, 28, "甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未"),        
    z(-227, 2, 28, "戊子 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 後 癸未"),        
    z(-226, 2, 28, "壬子 壬午 辛亥 辛巳 庚戌 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑"),        
    z(-225, 2, 28, "丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 壬申"),        
    z(-224, 2, 28, "辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 後 乙未"),        
    z(-223, 2, 28, "乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅"),        
    z(-222, 2, 28, "己未 己丑 戊午 戊子 丁巳 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申"),        
    z(-221, 2, 28, "甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 庚辰 己酉 己卯 後 戊申"),    
    z(-220, 2, 28, "戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅"),     
    z(-219, 2, 28, "壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉"),            
    z(-218, 2, 28, "丙寅 丙申 乙丑 乙未 甲子 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 後 辛酉"),        
    z(-217, 2, 28, "庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丁亥 丙辰 丙戌 乙卯"),        
    z(-216, 2, 28, "乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 後 庚辰"),        
    z(-215, 2, 28, "己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌"),        
    z(-214, 2, 28, "癸卯 癸酉 壬寅 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰"),        
    z(-213, 2, 28, "戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 乙丑 甲午 甲子 癸巳 癸亥 後 壬辰"),        
    z(-212, 2, 28, "壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌"),        
    z(-211, 2, 28, "丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳"),    
    z(-210, 2, 28, "庚戌 庚辰 己酉 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 後 乙巳"),      
    z(-209, 2, 28, "甲戌 甲辰 癸酉 癸卯 壬申 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥"),            
    z(-208, 2, 28, "己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 甲午 後 癸亥"),        
    z(-207, 2, 28, "癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳"),        
    z(-206, 2, 28, "丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子"),        
    z(-205, 2, 28, "辛巳 辛亥 庚辰 庚戌 己卯 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 後 丙子"),        
    z(-204, 2, 28, "乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 辛丑 庚午"),        
    z(-203, 2, 28, "庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子"),        
    z(-202, 2, 28, "甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 後 戊子"),        
    z(-201, 2, 28, "戊午 丁亥 丁巳 丙戌 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未"),    
    z(-200, 2, 28, "壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 己酉 戊寅 戊申 丁丑"),     
    z(-199, 2, 28, "丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 後 辛丑"),            
    z(-198, 2, 28, "辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未"),        
    z(-197, 2, 28, "乙丑 甲午 甲子 癸巳 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 後 己未"),        
    z(-196, 2, 28, "己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申 甲寅"),        
    z(-195, 2, 28, "癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申"),        
    z(-194, 2, 28, "戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 後 壬申"),        
    z(-193, 2, 28, "辛丑 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅"),        
    z(-192, 2, 28, "丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉"),        
    z(-191, 2, 28, "庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 後 乙酉"),    
    z(-190, 2, 28, "甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯"),
    z(-189, 2, 28, "戊申 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 後 癸卯"),            
    z(-188, 2, 28, "壬申 壬寅 辛未 辛丑 庚午 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉"),        
    z(-187, 2, 28, "丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 癸巳 壬戌 壬辰"),        
    z(-186, 2, 28, "辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 後 乙卯"),        
    z(-185, 2, 28, "乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌"),        
    z(-184, 2, 28, "己卯 己酉 戊寅 戊申 丁丑 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰"),        
    z(-183, 2, 28, "甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 庚子 己巳 己亥 後 戊辰"),        
    z(-182, 2, 28, "戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌"),        
    z(-181, 2, 28, "壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳"),    
    z(-180, 2, 28, "丙戌 丙辰 乙酉 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 後 辛巳"),
    z(-179, 2, 28, "庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丁未 丙子 丙午 乙亥"),            
    z(-178, 2, 28, "乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 後 己亥"),        
    z(-177, 2, 28, "己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳"),        
    z(-176, 2, 28, "癸亥 壬辰 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子"),        
    z(-175, 2, 28, "丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 甲寅 癸未 癸丑 壬午 後 壬子"),        
    z(-174, 2, 28, "辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午"),        
    z(-173, 2, 28, "丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子"),        
    z(-172, 2, 28, "庚午 己亥 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 後 甲子"),        
    z(-171, 2, 28, "甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 辛酉 庚寅 庚申 己丑 己未"),    
    z(-170, 2, 28, "戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 甲申 癸丑 後 癸未"),
    z(-169, 2, 28, "壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑"),            
    z(-168, 2, 28, "丙午 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未"),        
    z(-167, 2, 28, "辛丑 庚午 庚子 己巳 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 後 乙未"),        
    z(-166, 2, 28, "乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 辛卯 庚申 庚寅"),        
    z(-165, 2, 28, "己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申"),        
    z(-164, 2, 28, "癸丑 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 後 戊申"),        
    z(-163, 2, 28, "丁丑 丁未 丙子 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅"),        
    z(-162, 2, 28, "壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 後 丙寅"),        
    z(-161, 2, 28, "丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 辛酉"),    
    z(-160, 2, 28, "庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯"),
    z(-159, 2, 28, "甲申 甲寅 癸未 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 後 己卯"),            
    z(-158, 2, 28, "戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 乙巳 甲戌 甲辰 癸酉"),
    z(-157, 2, 28, "癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 戊辰"),            
    z(-156, 2, 28, "丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 後 辛卯"),        
    z(-155, 2, 28, "辛酉 庚寅 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌"),        
    z(-154, 2, 28, "乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 壬子 辛巳 辛亥 庚辰"),
    z(-153, 2, 28, "庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 乙亥 後 甲辰" ),        
    z(-152, 2, 28, "甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌"),        
    z(-151, 2, 28, "戊辰 丁酉 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 後 壬戌"),        
    z(-150, 2, 28, "壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 己未 戊子 戊午 丁亥 丁巳"),    
    z(-149, 2, 28, "丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 壬午 辛亥"),
    z(-148, 2, 28, "辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 後 乙亥"),            
    z(-147, 2, 28, "甲辰 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳"),        
    z(-146, 2, 28, "己亥 戊辰 戊戌 丁卯 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子"),        
    z(-145, 2, 28, "癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 己丑 戊午 後 戊子"),        
    z(-144, 2, 28, "丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午"),        
    z(-143, 2, 28, "辛亥 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子"),        
    z(-142, 2, 28, "丙午 乙亥 乙巳 甲戌 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 後 庚子"),        
    z(-141, 2, 28, "庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 丙申 乙丑 乙未"),        
    z(-140, 2, 28, "甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 後 己未"),            
    z(-139, 2, 28, "戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑"),        
    z(-138, 2, 28, "壬午 壬子 辛巳 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未"),        
    z(-137, 2, 28, "丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 後 辛未"),        
    z(-136, 2, 28, "辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 丙寅"),        
    z(-135, 2, 28, "乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申"),        
    z(-134, 2, 28, "己丑 己未 戊子 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 後 甲申"),        
    z(-133, 2, 28, "癸丑 癸未 壬子 壬午 辛亥 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅"),        
    z(-132, 2, 28, "戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 癸酉 後 壬寅"),    
    z(-131, 2, 28, "壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申"),
    z(-130, 2, 28, "丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯"),            
    z(-129, 2, 28, "庚申 庚寅 己未 己丑 戊午 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 後 乙卯"),        
    z(-128, 2, 28, "甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 庚辰 己酉"),        
    z(-127, 2, 28, "己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯"),        
    z(-126, 2, 28, "癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 後 丁卯"),        
    z(-125, 2, 28, "丁酉 丙寅 丙申 乙丑 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌"),        
    z(-124, 2, 28, "辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 戊子 丁巳 丁亥 丙辰 後 丙戌"),        
    z(-123, 2, 28, "乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰"),        
    z(-122, 2, 28, "庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌"),    
    z(-121, 2, 28, "甲辰 癸酉 癸卯 壬申 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 後 戊戌"),     
    z(-120, 2, 28, "戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 乙未 甲子 甲午 癸亥 癸巳"),            
    z(-119, 2, 28, "壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥"),        
    z(-118, 2, 28, "丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 後 辛亥"),        
    z(-117, 2, 28, "庚辰 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳"),        
    z(-116, 2, 28, "乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 壬寅 辛未 辛丑 庚午 庚子"),        
    z(-115, 2, 28, "己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 後 甲子"),        
    z(-114, 2, 28, "癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午"),        
    z(-113, 2, 28, "丁亥 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 後 壬午"),        
    z(-112, 2, 28, "辛亥 辛巳 庚戌 庚辰 己酉 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子"),    
    z(-111, 2, 28, "丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 壬申 辛丑 辛未"),      
    z(-110, 2, 28, "庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 後 甲午"),            
    z(-109, 2, 28, "甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑"),        
    z(-108, 2, 28, "戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未"),        
    z(-107, 2, 28, "癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 己卯 戊申 戊寅 後 丁未"),        
    z(-106, 2, 28, "丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑"),
    z(-105, 2, 28, "辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 後 乙丑"), 
    // There are no suffices for the second 十/十
    // 一/十二月 in history books (not mentioned in 資治通鑒).
    // We add them to distinguish with previous months.
    z(-104, 2, 28, "乙未 甲子 甲午 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 後後 戊子 後後 戊午 後後 丁亥"),
    y(-102, 2, 28, "丁巳 丙戌 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午"),        
    y(-101, 2, 28, "辛亥 辛巳 庚戌 庚辰 己酉 己卯 閏 戊申 戊寅 戊申 丁丑 丁未 丙子 丙午"),    
    y(-100, 2, 28, "乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 辛未 庚子"),    
    y(-99,  2, 28, "庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午"),            
    y(-98,  2, 28, "甲子 癸巳 癸亥 閏 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午"),        
    y(-97,  2, 28, "戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑"),        
    y(-96,  2, 28, "壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 戊寅 丁未 閏 丁丑"),        
    y(-95,  2, 28, "丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未"),        
    y(-94,  2, 28, "庚子 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑"),        
    y(-93,  2, 28, "乙未 甲子 甲午 癸亥 癸巳 癸亥 壬辰 壬戌 辛卯 閏 辛酉 庚寅 庚申 己丑"),        
    y(-92,  2, 28, "己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 乙酉 甲寅 甲申"),        
    y(-91,  2, 28, "癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅"),    
    y(-90,  2, 28, "戊申 丁丑 丁未 丙子 丙午 閏 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅"),
    y(-89,  2, 28, "辛未 辛丑 庚午 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申"),            
    y(-88,  2, 28, "丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 壬辰 辛酉 辛卯"),        
    y(-87,  2, 28, "庚申 閏 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 乙卯"),        
    y(-86,  2, 28, "甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉"),        
    y(-85,  2, 28, "戊寅 戊申 丁丑 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 閏 甲戌 癸卯 癸酉"),        
    y(-84,  2, 28, "壬寅 壬申 辛丑 辛未 庚子 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯"),        
    y(-83,  2, 28, "丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 壬戌"),        
    y(-82,  2, 28, "辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 閏 戊午 丁亥 丁巳 丙戌 丙辰 乙酉"),        
    y(-81,  2, 28, "乙卯 甲申 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰"),    
    y(-80,  2, 28, "己酉 己卯 戊申 戊寅 丁未 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌"),    
    y(-79,  2, 28, "甲辰 癸酉 癸卯 閏 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 己巳 戊戌"),            
    y(-78,  2, 28, "戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰"),        
    y(-77,  2, 28, "壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 閏 丁亥 丙辰"),        
    y(-76,  2, 28, "丙戌 乙卯 乙酉 甲寅 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥"),        
    y(-75,  2, 28, "庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 丙子 乙巳"),        
    y(-74,  2, 28, "乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 閏 辛未 庚子 庚午 己亥 己巳"),        
    y(-73,  2, 28, "己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥"),        
    y(-72,  2, 28, "癸巳 壬戌 壬辰 辛酉 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午"),        
    y(-71,  2, 28, "丁亥 丁巳 丙戌 丙辰 乙酉 閏 乙卯 甲申 甲寅 甲申 癸丑 癸未 壬子 壬午"),    
    y(-70,  2, 28, "辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子"),
    y(-69,  2, 28, "丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午"),            
    y(-68,  2, 28, "庚子 閏 己巳 己亥 戊辰 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午"),        
    y(-67,  2, 28, "甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 辛卯 庚申 庚寅 己未 己丑"),        
    y(-66,  2, 28, "戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 閏 甲申 癸丑 癸未 癸丑"),        
    y(-65,  2, 28, "壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未"),        
    y(-64,  2, 28, "丙子 丙午 乙亥 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑"),        
    y(-63,  2, 28, "辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 閏 戊戌 丁卯 丁酉 丙寅 丙申 乙丑"),        
    y(-62,  2, 28, "乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 庚申"),        
    y(-61,  2, 28, "己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅"),    
    y(-60,  2, 28, "癸未 癸丑 癸未 壬子 閏 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅"),
    y(-59,  2, 28, "丁未 丁丑 丙午 丙子 乙巳 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申"),            
    y(-58,  2, 28, "壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 戊辰 丁酉 丁卯 閏 丙申"),        
    y(-57,  2, 28, "丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅"),        
    y(-56,  2, 28, "庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉"),        
    y(-55,  2, 28, "甲寅 甲申 癸丑 癸未 壬子 壬午 壬子 辛巳 閏 辛亥 庚辰 庚戌 己卯 己酉"),        
    y(-54,  2, 28, "戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 乙亥 甲辰 甲戌 癸卯"),        
    y(-53,  2, 28, "癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉"),        
    y(-52,  2, 28, "丁卯 丁酉 丙寅 丙申 乙丑 閏 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉"),        
    y(-51,  2, 28, "辛卯 庚申 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰"),    
    y(-50,  2, 28, "乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 壬午 辛亥 辛巳 庚戌"),
    y(-49,  2, 28, "庚辰 己酉 閏 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌"),            
    y(-48,  2, 28, "甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰"),        
    y(-47,  2, 28, "戊戌 丁卯 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 閏 癸巳 癸亥 壬辰"),        
    y(-46,  2, 28, "壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 己丑 戊午 戊子 丁巳 丁亥"),        
    y(-45,  2, 28, "丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 壬子 辛巳"),        
    y(-44,  2, 28, "辛亥 庚辰 庚戌 己卯 己酉 戊寅 閏 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳"),        
    y(-43,  2, 28, "甲戌 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥"),        
    y(-42,  2, 28, "己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 丙申 乙丑 乙未 甲子 甲午"),        
    y(-41,  2, 28, "癸亥 癸巳 壬戌 閏 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 己未 戊子 戊午"),    
    y(-40,  2, 28, "丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子"),
    y(-39,  2, 28, "辛巳 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 閏 丙子"),            
    y(-38,  2, 28, "乙巳 乙亥 甲辰 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午"),        
    y(-37,  2, 28, "庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 丙寅 乙未 乙丑"),        
    y(-36,  2, 28, "甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 閏 庚寅 庚申 己丑 己未 戊子"),        
    y(-35,  2, 28, "戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未"),        
    y(-34,  2, 28, "壬子 壬午 辛亥 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑"),        
    y(-33,  2, 28, "丁未 丙子 丙午 乙亥 閏 乙巳 甲戌 甲辰 癸酉 癸卯 癸酉 壬寅 壬申 辛丑"),        
    y(-32,  2, 28, "辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 丙申"),        
    y(-31,  2, 28, "乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅"),    
    y(-30,  2, 28, "己未 閏 己丑 戊午 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅"),
    y(-29,  2, 28, "癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 庚辰 己酉 己卯 戊申"),            
    y(-28,  2, 28, "戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 閏 癸酉 癸卯 壬申"),        
    y(-27,  2, 28, "壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅"),        
    y(-26,  2, 28, "丙申 乙丑 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉"),        
    y(-25,  2, 28, "庚寅 庚申 己丑 己未 戊子 戊午 閏 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉"),
    y(-24,  2, 28, "甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 庚戌 己卯"),        
    y(-23,  2, 28, "己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉"),        
    y(-22,  2, 28, "癸卯 壬申 壬寅 閏 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉"),        
    y(-21,  2, 28, "丁卯 丙申 丙寅 乙未 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰"),    
    y(-20,  2, 28, "辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丁巳 丙戌 閏 丙辰"),      
    y(-19,  2, 28, "乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌"),            
    y(-18,  2, 28, "庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰"),        
    y(-17,  2, 28, "甲戌 癸卯 癸酉 壬寅 壬申 壬寅 辛未 辛丑 庚午 閏 庚子 己巳 己亥 戊辰"),        
    y(-16,  2, 28, "戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 甲子 癸巳 癸亥"),        
    y(-15,  2, 28, "壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳"),        
    y(-14,  2, 28, "丁亥 丙辰 丙戌 乙卯 乙酉 閏 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳"),        
    y(-13,  2, 28, "庚戌 庚辰 己酉 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥"),        
    y(-12,  2, 28, "乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 壬申 辛丑 辛未 庚子 庚午"),        
    y(-11,  2, 28, "己亥 閏 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 甲午"),    
    y(-10,  2, 28, "癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子"),    
    y(-9,   2, 28, "丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 閏 癸丑 壬午 壬子"),            
    y(-8,   2, 28, "辛巳 辛亥 庚辰 庚戌 己卯 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午"),        
    y(-7,   2, 28, "丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 辛丑"),        
    y(-6,   2, 28, "庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 閏 丁酉 丙寅 丙申 乙丑 乙未 甲子"),        
    y(-5,   2, 28, "甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未"),        
    y(-4,   2, 28, "戊子 戊午 丁亥 丁巳 丙戌 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑"),        
    y(-3,   2, 28, "癸未 壬子 壬午 閏 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 戊申 丁丑"),        
    y(-2,   2, 28, "丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未"),        
    y(-1,   2, 28, "辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 閏 丙寅 乙未"),    
    y(0,    2, 28, "乙丑 甲午 甲子 癸巳 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅")
  )

  private val ce222 = y(222, 1, 30, "丙寅 丙申 乙丑 乙未 甲子 甲午 閏 癸亥 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉")
  private val ce223 = y(223, 2, 18, "庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 丙戌 乙卯")
  private val ce224 = y(224, 2, 8,  "乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉")
  private val ce225 = y(225, 1, 27, "己卯 戊申 戊寅 閏 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉")
  private val ce226 = y(226, 2, 15, "癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 庚午 己亥 己巳 戊戌 戊辰")
  private val ce227 = y(227, 2, 4,  "丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 癸巳 壬戌 閏 壬辰")
  private val ce228 = y(228, 2, 23, "辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌")
  private val ce229 = y(229, 2, 11, "乙卯 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰")    
  private val ce230 = y(230, 2, 1,  "庚戌 己卯 己酉 戊寅 戊申 戊寅 丁未 丁丑 丙午 閏 丙子 乙巳 乙亥 甲辰") 
  private val ce231 = y(231, 2, 20, "甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 庚子 己巳 己亥")
  private val ce232 = y(232, 2, 9,  "戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳") 
  private val ce233 = y(233, 1, 28, "壬戌 壬辰 壬戌 辛卯 辛酉 閏 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳")
  private val ce234 = y(234, 2, 16, "丙戌 丙辰 乙酉 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥")
  private val ce235 = y(235, 2, 6,  "辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丁未 丙子 丙午")
  private val ce236 = y(236, 1, 26, "乙亥 閏 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 庚午")
  private val ce240 = y(240, 2, 10, "辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子")
  private val ce247 = y(247, 2, 22, "庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 丙申 乙丑")
  private val ce248 = y(248, 2, 12, "乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未")
  private val ce261 = y(261, 2, 17, "己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌")
  private val ce265 = y(265, 2, 3,  "丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 閏 辛巳 庚戌")
  private val ce269 = y(269, 2, 19, "癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳")
  private val ce273 = y(273, 2, 5,  "庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子")
  private val ce275 = y(275, 2, 13, "戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 乙酉 甲寅 甲申 癸丑 癸未")
  private val ce277 = y(277, 2, 20, "丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑")
  private val ce278 = y(278, 2, 9,  "庚午 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未") 
  private val ce279 = y(279, 1, 30, "乙丑 甲午 甲子 癸巳 癸亥 壬辰 閏 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未")
  private val ce280 = y(280, 2, 18, "己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 甲寅")
  private val ce440 = y(440, 2, 19, "庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯")
  private val ce441 = y(441, 2, 7,  "甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉")
  private val ce442 = y(442, 1, 27, "戊寅 戊申 戊寅 丁未 丁丑 閏 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉")
  private val ce443 = y(443, 2, 15, "壬寅 壬申 辛丑 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯")
  private val ce444 = y(444, 2, 5,  "丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 癸亥 壬辰 壬戌")
  private val ce446 = y(446, 2, 12, "乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰")
  private val ce449 = y(449, 2, 9,  "戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰")    
  private val ce451 = y(451, 2, 17, "丙戌 乙卯 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥")
  private val ce452 = y(452, 2, 6,  "庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丁丑 丙午 丙子 乙巳")
  private val ce453 = y(453, 1, 26, "乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 閏 壬申 辛丑 辛未 庚子 庚午 己亥 己巳")
  private val ce454 = y(454, 2, 14, "己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥")
  private val ce457 = y(457, 2, 10, "辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子")
  private val ce458 = y(458, 1, 31, "丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 閏 庚子")
  private val ce459 = y(459, 2, 18, "己巳 己亥 戊辰 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午")
  private val ce460 = y(460, 2, 8,  "甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 辛卯 庚申 庚寅 己未 己丑")
  private val ce461 = y(461, 1, 27, "戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 閏 甲申 癸丑 癸未 癸丑")
  private val ce462 = y(462, 2, 15, "壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未")
  private val ce463 = y(463, 2, 4,  "丙子 丙午 乙亥 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑")
  private val ce465 = y(465, 2, 12, "乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 庚申")
  private val ce466 = y(466, 2, 1,  "己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅")
  private val ce467 = y(467, 1, 21, "癸未 閏 癸丑 壬午 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅")
  private val ce468 = y(468, 2, 9,  "丁未 丁丑 丙午 丙子 乙巳 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申")
  private val ce470 = y(470, 2, 17, "丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅")
  private val ce471 = y(471, 2, 6,  "庚申 己丑 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉")
  private val ce473 = y(473, 2, 13, "戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 甲戌 癸卯")
  private val ce474 = y(474, 2, 3,  "癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉")
  private val ce475 = y(475, 1, 23, "丁卯 丙申 丙寅 閏 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉")
  private val ce476 = y(476, 2, 11, "辛卯 庚申 庚寅 己未 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰")
  private val ce478 = y(478, 2, 18, "己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌")
  private val ce479 = y(479, 2, 7,  "癸卯 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰")
  private val ce480 = y(480, 1, 28, "戊戌 丁卯 丁酉 丙寅 丙申 丙寅 乙未 乙丑 甲午 閏 甲子 癸巳 癸亥 壬辰")
  private val ce481 = y(481, 2, 15, "壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 戊子 丁巳 丁亥")
  private val ce482 = y(482, 2, 4,  "丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳") 
  private val ce484 = y(484, 2, 12, "甲戌 甲辰 癸酉 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥")
  private val ce485 = y(485, 2, 1,  "己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 乙未 甲子 甲午")
  private val ce486 = y(486, 1, 21, "癸亥 閏 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳") 
  private val ce487 = y(487, 2, 9,  "丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子")    
  private val ce489 = y(489, 2, 16, "乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 壬寅 辛未 辛丑 庚午")
  private val ce490 = y(490, 2, 6,  "庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子") 
  private val ce491 = y(491, 1, 26, "甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 閏 辛酉 庚寅 庚申 己丑 己未 戊子")
  private val ce492 = y(492, 2, 14, "戊午 丁亥 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未") 
  private val ce493 = y(493, 2, 2,  "壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 己酉 戊寅 戊申 丁丑")
  private val ce494 = y(494, 1, 23, "丁未 丙子 丙午 乙亥 閏 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑") 
  private val ce495 = y(495, 2, 11, "辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未")
  private val ce497 = y(497, 2, 18, "己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申 甲寅")
  private val ce498 = y(498, 2, 7,  "癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申") 
  private val ce499 = y(499, 1, 28, "戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 閏 甲戌 癸卯 癸酉 壬寅 壬申")
  private val ce500 = y(500, 2, 15, "辛丑 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅")
  private val ce501 = y(501, 2, 4,  "丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉")
  private val ce502 = y(502, 1, 24, "庚寅 庚申 己丑 己未 閏 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 乙酉")     
  private val CEYears = Array(
    // Since we use propletic Julian calendar, add one day for years CE 1,2,3,4.
    y(1,  2, 12, "己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申"), 
    y(2,  2, 2,  "甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 閏 庚戌 己卯 己酉 戊寅 戊申"),
    y(3,  2, 21, "戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅"),
    y(4,  2, 10, "壬申 辛丑 辛未 庚子 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉"), 
    y(5,  1, 29, "丙寅 丙申 乙丑 乙未 甲子 閏 甲午 癸亥 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉"), 
    y(6,  2, 17, "庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯"),
    y(7,  2, 7,  "乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉"),
    y(8,  1, 27, "己卯 閏 戊申 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉"),
    // 王莽改元，用殷正（建醜）。但通鑑仍用夏正（建寅），因此數據扔按建寅。
    y(9,  2, 14, "癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 庚午 己亥 己巳 戊戌 戊辰"), 
    y(10, 2, 3,  "丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 閏 癸亥 壬辰 壬戌 壬辰"), 
    y(11, 2, 22, "辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌"), 
    y(12, 2, 11, "乙卯 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰"), 
    y(13, 1, 31, "庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 閏 丁丑 丙午 丙子 乙巳 乙亥 甲辰"),
    y(14, 2, 19, "甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 庚子 己巳 己亥"), 
    y(15, 2, 8,  "戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳"),
    y(16, 1, 28, "壬戌 壬辰 壬戌 辛卯 閏 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳"), 
    y(17, 2, 15, "丙戌 丙辰 乙酉 乙卯 甲申 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥"),
    y(18, 2, 5,  "辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丁未 丙子 丙午 閏 乙亥"), 
    y(19, 2, 24, "乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳"),
    y(20, 2, 13, "己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子"), 
    y(21, 2, 1,  "癸巳 癸亥 壬辰 壬戌 壬辰 辛酉 辛卯 庚申 閏 庚寅 己未 己丑 戊午 戊子"),
    y(22, 2, 20, "丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 甲寅 癸未 癸丑 壬午"), 
    y(23, 2, 10, "壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子"),
    y(24, 1, 30, "丙午 丙子 乙巳 乙亥 甲辰 閏 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子"), 
    y(25, 2, 17, "庚午 己亥 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未"),
    y(26, 2, 6,  "甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 辛酉 庚寅 庚申 己丑"), 
    y(27, 1, 27, "己未 戊子 閏 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 甲申 癸丑"),
    y(28, 2, 15, "癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未"), 
    y(29, 2, 3,  "丁丑 丙午 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 閏 癸卯 壬申 壬寅 辛未"),
    y(30, 2, 22, "辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 戊辰 丁酉 丁卯 丙申 丙寅"), 
    y(31, 2, 11, "乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 辛卯 庚申"),
    y(32, 2, 1,  "庚寅 己未 己丑 戊午 戊子 丁巳 閏 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申"), 
    y(33, 2, 18, "癸丑 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅"),
    y(34, 2, 8,  "戊申 丁丑 丁未 丙子 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉"), 
    y(35, 1, 28, "壬寅 壬申 辛丑 閏 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 戊戌 丁卯 丁酉"),
    y(36, 2, 16, "丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯"), 
    y(37, 2, 4,  "庚申 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 閏 乙卯"),
    y(38, 2, 23, "甲申 甲寅 癸未 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉"), 
    y(39, 2, 13, "己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 乙巳 甲戌 甲辰"),
    y(40, 2, 2,  "癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 閏 己巳 己亥 戊辰 戊戌 戊辰"), 
    y(41, 2, 20, "丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌"),
    y(42, 2, 9,  "辛卯 辛酉 庚寅 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰"), 
    y(43, 1, 30, "丙戌 乙卯 乙酉 甲寅 閏 甲申 癸丑 癸未 壬子 壬午 壬子 辛巳 辛亥 庚辰"),
    y(44, 2, 18, "庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 乙亥"), 
    y(45, 2, 6,  "甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳"),
    y(46, 1, 26, "戊戌 閏 戊辰 丁酉 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳"), 
    y(47, 2, 14, "壬戌 壬辰 辛酉 辛卯 庚申 庚寅 庚申 己丑 己未 戊子 戊午 丁亥"),
    y(48, 2, 4,  "丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 閏 壬子 壬午 辛亥"), 
    y(49, 2, 22, "辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳"),
    y(50, 2, 11, "乙亥 甲辰 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子"), 
    y(51, 1, 31, "己巳 己亥 戊辰 戊戌 丁卯 丁酉 閏 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子"),
    y(52, 2, 19, "癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 己丑 戊午"), 
    y(53, 2, 8,  "戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子"),
    y(54, 1, 28, "壬午 壬子 辛巳 閏 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子"), 
    y(55, 2, 16, "丙午 乙亥 乙巳 甲戌 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未"),
    y(56, 2, 5,  "庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 丙申 乙丑 閏 乙未"), 
    y(57, 2, 23, "甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑"),
    y(58, 2, 13, "己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未"), 
    y(59, 2, 2,  "癸丑 壬午 壬子 辛巳 辛亥 辛巳 庚戌 庚辰 己酉 閏 己卯 戊申 戊寅 丁未"),
    y(60, 2, 21, "丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 甲辰 癸酉 癸卯 壬申 壬寅"),
    y(61, 2, 9,  "辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申"),
    y(62, 1, 30, "丙寅 乙未 乙丑 甲午 甲子 閏 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申"), 
    y(63, 2, 17, "己丑 己未 戊子 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅"),
    y(64, 2, 7,  "甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 辛亥 庚辰 庚戌 己卯 己酉"), 
    y(65, 1, 26, "戊寅 閏 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 癸酉"),
    y(66, 2, 14, "壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯"), 
    y(67, 2, 3,  "丙申 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 閏 壬辰 辛酉 辛卯"),
    y(68, 2, 22, "庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉"), 
    y(69, 2, 11, "乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 庚辰"),
    y(70, 1, 31, "己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 閏 丙子 乙巳 乙亥 甲辰 甲戌 癸卯"), 
    y(71, 2, 19, "癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌"),
    y(72, 2, 8,  "丁卯 丁酉 丙寅 丙申 乙丑 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰"), 
    y(73, 1, 28, "壬戌 辛卯 辛酉 閏 庚寅 庚申 己丑 己未 戊子 戊午 戊子 丁巳 丁亥 丙辰"),
    y(74, 2, 16, "丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌"), 
    y(75, 2, 5,  "庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 閏 乙巳 甲戌"),
    y(76, 2, 24, "甲辰 癸酉 癸卯 壬申 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳"), 
    y(77, 2, 12, "戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 乙未 甲子 甲午 癸亥"),
    y(78, 2, 2,  "癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 閏 己丑 戊午 戊子 丁巳 丁亥"), 
    y(79, 2, 21, "丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳"),
    y(80, 2, 10, "辛亥 庚辰 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子"),
    y(81, 1, 29, "乙巳 乙亥 甲辰 甲戌 癸卯 閏 癸酉 壬寅 壬申 壬寅 辛未 辛丑 庚午 庚子"),
    y(82, 2, 17, "己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午"), 
    y(83, 2, 7,  "甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子"),
    y(84, 1, 27, "戊午 閏 丁亥 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子"), 
    y(85, 2, 13, "辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午"),
    y(86, 2, 2,  "乙亥 乙巳 甲戌 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 閏 辛未 庚子 庚午"), 
    y(87, 2, 21, "己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 丙申 乙丑 乙未 甲子"),
    y(88, 2, 11, "甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 己未"), 
    y(89, 1, 30, "戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 閏 乙卯 甲申 甲寅 癸未 癸丑 壬午"),
    y(90, 2, 18, "壬子 辛巳 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑"), 
    y(91, 2, 7,  "丙午 丙子 乙巳 乙亥 甲辰 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未"),
    y(92, 1, 28, "辛丑 庚午 庚子 閏 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 丙寅 乙未"), 
    y(93, 2, 15, "乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑"),
    y(94, 2, 4,  "己未 戊子 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 閏 甲申 癸丑"), 
    y(95, 2, 23, "癸未 壬子 壬午 辛亥 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申"),
    y(96, 2, 12, "丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 癸酉 壬寅"), 
    y(97, 2, 1,  "壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 閏 戊辰 丁酉 丁卯 丙申 丙寅"),
    y(98, 2, 20, "丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申"), 
    y(99, 2, 9,  "庚寅 己未 己丑 戊午 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯"),
    y(100, 1, 29, "甲申 甲寅 癸未 癸丑 壬午 閏 壬子 辛巳 辛亥 庚辰 庚戌 庚辰 己酉 己卯"),
    y(101, 2, 16, "戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉"),
    y(102, 2, 6,  "癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯"), 
    y(103, 1, 26, "丁酉 閏 丙寅 丙申 乙丑 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯"),
    y(104, 2, 14, "辛酉 庚寅 庚申 己丑 己未 戊子 戊午 戊子 丁巳 丁亥 丙辰 丙戌"), 
    y(105, 2, 2,  "乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 閏 辛巳 庚戌 庚辰 庚戌"),
    y(106, 2, 21, "己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰"), 
    y(107, 2, 10, "癸酉 癸卯 壬申 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌"),
    y(108, 1, 31, "戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 閏 乙未 甲子 甲午 癸亥 癸巳 壬戌"), 
    y(109, 2, 18, "壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丁巳"),
    y(110, 2, 7,  "丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥"), 
    y(111, 1, 27, "庚辰 庚戌 庚辰 己酉 閏 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥"),
    y(112, 2, 15, "甲辰 甲戌 癸卯 癸酉 壬寅 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳"), 
    y(113, 2, 4,  "己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 甲子 閏 癸巳"),
    y(114, 2, 23, "癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥"), 
    y(115, 2, 12, "丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午"),
    y(116, 2, 1,  "辛亥 辛巳 庚戌 庚辰 己酉 己卯 己酉 戊寅 閏 戊申 丁丑 丁未 丙子 丙午"), 
    y(117, 2, 19, "乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 壬申 辛丑 辛未 庚子"),
    y(118, 2, 9,  "庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午"), 
    y(119, 1, 29, "甲子 甲午 癸亥 癸巳 壬戌 閏 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午"),
    y(120, 2, 17, "戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑"), 
    y(121, 2, 5,  "壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 己卯 戊申 戊寅 丁未"),
    y(122, 1, 26, "丁丑 丙午 閏 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未"), 
    y(123, 2, 14, "辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑"),
    y(124, 2, 3,  "乙未 甲子 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 閏 庚寅 庚申 己丑"), 
    y(125, 2, 21, "己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 丙戌 乙卯 乙酉 甲寅 甲申"),
    y(126, 2, 10, "癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅"), 
    y(127, 1, 31, "戊申 丁丑 丁未 丙子 丙午 乙亥 閏 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅"),
    y(128, 2, 18, "辛未 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申"), 
    y(129, 2, 7,  "丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯"),
    y(130, 1, 27, "庚申 庚寅 己未 閏 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯"), 
    y(131, 2, 15, "甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉"),
    y(132, 2, 4,  "戊寅 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 閏 癸酉"), 
    y(133, 2, 22, "壬寅 壬申 辛丑 辛未 庚子 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯"),
    y(134, 2, 12, "丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 癸亥 壬辰 壬戌"), 
    y(135, 2, 1,  "辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 閏 丁亥 丁巳 丙戌 丙辰 乙酉"),
    y(136, 2, 20, "乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰"), 
    y(137, 2, 8,  "己酉 己卯 戊申 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌"),
    y(138, 1, 29, "甲辰 癸酉 癸卯 壬申 閏 壬寅 辛未 辛丑 庚午 庚子 庚午 己亥 己巳 戊戌"), 
    y(139, 2, 17, "戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰"),
    y(140, 2, 6,  "壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥"), 
    y(141, 1, 25, "丙辰 閏 丙戌 乙卯 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥"),
    y(142, 2, 13, "庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丁丑 丙午 丙子 乙巳"), 
    y(143, 2, 3,  "乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 閏 庚午 庚子 己巳"),
    y(144, 2, 22, "己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥"), 
    y(145, 2, 10, "癸巳 壬戌 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午"),
    y(146, 1, 30, "丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 閏 甲申 甲寅 甲申 癸丑 癸未 壬子 壬午"), 
    y(147, 2, 18, "辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丁未 丙子"),
    y(148, 2, 8,  "丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午"), 
    y(149, 1, 27, "庚子 己巳 己亥 閏 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午"),
    y(150, 2, 15, "甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 辛卯 庚申 庚寅 己未 己丑"), 
    y(151, 2, 4,  "戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 甲寅 癸未 閏 癸丑"),
    y(152, 2, 23, "壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未"), 
    y(153, 2, 11, "丙子 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑"),
    y(154, 2, 1,  "辛未 庚子 庚午 己亥 己巳 己亥 戊辰 戊戌 丁卯 閏 丁酉 丙寅 丙申 乙丑"), 
    y(155, 2, 20, "乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 辛酉 庚寅 庚申"),
    y(156, 2, 9,  "己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅"), 
    y(157, 1, 28, "癸未 癸丑 癸未 壬子 壬午 閏 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅"),
    y(158, 2, 16, "丁未 丁丑 丙午 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申"), 
    y(159, 2, 6,  "壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 戊辰 丁酉 丁卯"),
    y(160, 1, 26, "丙申 閏 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 辛卯"),
    y(161, 2, 13, "庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉"),
    y(162, 2, 2,  "甲寅 甲申 癸丑 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 閏 庚戌 己卯 己酉"), 
    y(163, 2, 21, "戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 乙亥 甲辰 甲戌 癸卯"),
    y(164, 2, 11, "癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 戊戌"), 
    y(165, 1, 30, "丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 閏 甲午 癸亥 癸巳 壬戌 壬辰 辛酉"),
    y(166, 2, 18, "辛卯 庚申 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰"), 
    y(167, 2, 7,  "乙酉 乙卯 甲申 甲寅 癸未 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌"),
    y(168, 1, 28, "庚辰 己酉 己卯 閏 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 乙巳 甲戌"), 
    y(169, 2, 15, "甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰"),
    y(170, 2, 4,  "戊戌 丁卯 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 閏 癸亥 壬辰"), 
    y(171, 2, 23, "壬戌 辛卯 辛酉 庚寅 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥"),
    y(172, 2, 12, "丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 壬子 辛巳"), 
    y(173, 2, 1,  "辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 閏 丁未 丙子 丙午 乙亥 乙巳"),
    y(174, 2, 20, "乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥"), 
    y(175, 2, 9,  "己巳 戊戌 戊辰 丁酉 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午"),
    y(176, 1, 29, "癸亥 癸巳 壬戌 壬辰 辛酉 閏 辛卯 庚申 庚寅 己未 己丑 己未 戊子 戊午"), 
    y(177, 2, 16, "丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子"),
    y(178, 2, 6,  "壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午"), 
    y(179, 1, 26, "丙子 閏 乙巳 乙亥 甲辰 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午"),
    y(180, 2, 14, "庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丁卯 丙申 丙寅 乙未 乙丑"),
    y(181, 2, 2,  "甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 閏 庚申 己丑 己未 己丑"),
    y(182, 2, 21, "戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未"), 
    y(183, 2, 10, "壬子 壬午 辛亥 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑"),
    y(184, 1, 31, "丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 閏 甲戌 癸卯 癸酉 壬寅 壬申 辛丑"), 
    y(185, 2, 18, "辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 丙申"),
    y(186, 2, 7,  "乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅"), 
    y(187, 1, 27, "己未 己丑 己未 戊子 閏 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅"),
    y(188, 2, 15, "癸未 癸丑 壬午 壬子 辛巳 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申"), 
    y(189, 2, 4,  "戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 癸卯 閏 壬申"),
    y(190, 2, 23, "壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅"), 
    y(191, 2, 12, "丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉"),
    y(192, 2, 1,  "庚寅 庚申 己丑 己未 戊子 戊午 戊子 丁巳 閏 丁亥 丙辰 丙戌 乙卯 乙酉"), 
    y(193, 2, 19, "甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 辛亥 庚辰 庚戌 己卯"),
    y(194, 2, 9,  "己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉"), 
    y(195, 1, 29, "癸卯 癸酉 壬寅 壬申 辛丑 閏 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉"),
    y(196, 2, 17, "丁卯 丙申 丙寅 乙未 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰"), 
    y(197, 2, 5,  "辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 戊午 丁亥 丁巳 丙戌"),
    y(198, 1, 26, "丙辰 乙酉 閏 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌"), 
    y(199, 2, 14, "庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰"),
    y(200, 2, 3,  "甲戌 癸卯 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 閏 己巳 己亥 戊辰"),
    y(201, 2, 21, "戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 乙丑 甲午 甲子 癸巳 癸亥"),
    y(202, 2, 10, "壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳"), 
    y(203, 1, 31, "丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 閏 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳"),
    y(204, 2, 18, "庚戌 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥"), 
    y(205, 2, 7,  "乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 壬申 辛丑 辛未 庚子 庚午"),
    y(206, 1, 27, "己亥 己巳 戊戌 閏 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 乙未 甲子 甲午"), 
    y(207, 2, 15, "癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子"),
    y(208, 2, 4,  "丁巳 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 閏 壬子"), 
    y(209, 2, 22, "辛巳 辛亥 庚辰 庚戌 己卯 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午"),
    y(210, 2, 12, "丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 壬寅 辛未 辛丑"), 
    y(211, 2, 1,  "庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 閏 丙寅 丙申 乙丑 乙未 甲子"),
    y(212, 2, 20, "甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未"), 
    y(213, 2, 8,  "戊子 戊午 丁亥 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑"),
    y(214, 1, 29, "癸未 壬子 壬午 辛亥 閏 辛巳 庚戌 庚辰 己酉 己卯 己酉 戊寅 戊申 丁丑"), 
    y(215, 2, 17, "丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未"),
    y(216, 2, 6,  "辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅"), 
    y(217, 1, 25, "乙未 閏 乙丑 甲午 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅"),
    y(218, 2, 13, "己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申"), 
    y(219, 2, 3,  "甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 閏 己酉 己卯 戊申"),
    y(220, 2, 22, "戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅"), 
    y(221, 2, 10, "壬申 辛丑 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉"),
    ce222, ce223, ce224, ce225, ce226, ce227, ce228, ce229, ce230, ce231, ce232,
    ce233, ce234, ce235, ce236,
    y(237, 2, 13, "己亥 己巳 進 戊戌 戊辰 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午"), // 三月，魏改元“景初”，建丑，以三月为四月，十二月为正月
    y(238, 1, 3,  "癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 庚申 己丑 己未 閏 戊子 戊午"), 
    y(239, 1, 22, "丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 後 壬午"),
    ce240,
    y(241, 1, 29, "乙巳 乙亥 甲辰 甲戌 甲辰 癸酉 閏 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子"),
    y(242, 2, 17, "己巳 己亥 戊辰 戊戌 丁卯 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午"), 
    y(243, 2, 7,  "甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 己丑"),
    y(244, 1, 27, "戊午 戊子 丁巳 閏 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子"), 
    y(245, 2, 14, "壬午 辛亥 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未"),
    y(246, 2, 3,  "丙子 丙午 乙亥 乙巳 甲戌 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 閏 辛未"),
    ce247, ce248,
    y(249, 1, 31, "己丑 戊午 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 閏 乙卯 甲申 甲寅 癸未"),
    y(250, 2, 19, "癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 庚辰 己酉 己卯 戊申 戊寅"), 
    y(251, 2, 8,  "丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 癸卯 壬申"),
    y(252, 1, 29, "壬寅 辛未 辛丑 庚午 庚子 閏 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申"), 
    y(253, 2, 15, "乙丑 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅"),
    y(254, 2, 5,  "庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉"), 
    y(255, 1, 25, "甲寅 閏 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 庚戌 己卯 己酉"),
    // For year CE 256, see comment in CE Years.
    y(256, 2, 13, "戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯"),    
    y(257, 2, 1,  "壬申 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 閏 戊辰 丁酉 丁卯"),
    y(258, 2, 20, "丙申 丙寅 乙未 乙丑 甲午 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉"), 
    y(259, 2, 10, "辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丁巳 丙戌 丙辰"),
    y(260, 1, 30, "乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 閏 壬子 辛巳 辛亥 庚辰 庚戌 己卯"),
    ce261,
    y(262, 2, 6,  "癸卯 癸酉 壬寅 壬申 辛丑 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰"), 
    y(263, 1, 27, "戊戌 丁卯 丁酉 閏 丙寅 丙申 乙丑 乙未 甲子 甲午 甲子 癸巳 癸亥 壬辰"),
    y(264, 2, 15, "壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌"),
    ce265,
    y(266, 2, 22, "庚辰 己酉 己卯 戊申 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳"), 
    y(267, 2, 11, "甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 辛未 庚子 庚午 己亥"),
    y(268, 2, 1,  "己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 閏 乙丑 甲午 甲子 癸巳 癸亥"),
    ce269,
    y(270, 2, 8,  "丁亥 丙辰 丙戌 乙卯 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子"), 
    y(271, 1, 28, "辛巳 辛亥 庚辰 庚戌 己卯 閏 己酉 戊寅 戊申 戊寅 丁未 丁丑 丙午 丙子"),
    y(272, 2, 16, "乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午"),
    ce273,
    y(274, 1, 25, "甲午 閏 癸亥 癸巳 壬戌 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子"),
    ce275,
    y(276, 2, 2,  "壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 閏 戊寅 丁未 丁丑 丁未"),
    ce277, ce278, ce279, ce280,
    y(281, 2, 6,  "癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申"),
    y(282, 1, 26, "丁丑 丁未 丙子 丙午 閏 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申"), 
    y(283, 2, 14, "辛丑 辛未 庚子 庚午 己亥 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅"),
    y(284, 2, 4,  "丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 辛酉 閏 庚寅"), 
    y(285, 2, 22, "庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申"),
    y(286, 2, 11, "甲寅 癸未 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯"), 
    y(287, 1, 31, "戊申 戊寅 丁未 丁丑 丙午 丙子 丙午 乙亥 閏 乙巳 甲戌 甲辰 癸酉 癸卯"),
    y(288, 2, 19, "壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 戊辰 丁酉"), 
    y(289, 2, 8,  "丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯"),
    y(290, 1, 28, "辛酉 庚寅 庚申 庚寅 己未 閏 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯"), 
    y(291, 2, 16, "乙酉 甲寅 甲申 癸丑 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌"),
    y(292, 2, 5,  "己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 乙亥 甲辰"), 
    y(293, 1, 25, "甲戌 癸卯 閏 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰"),
    y(294, 2, 12, "丁酉 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌"), 
    y(295, 2, 2,  "壬辰 辛酉 辛卯 庚申 庚寅 庚申 己丑 己未 戊子 戊午 閏 丁亥 丁巳 丙戌"),
    y(296, 2, 21, "丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 壬午 辛亥 辛巳"), 
    y(297, 2, 9,  "庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥"),
    y(298, 1, 29, "甲辰 甲戌 甲辰 癸酉 癸卯 壬申 閏 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥"), 
    y(299, 2, 17, "戊辰 戊戌 丁卯 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳"),
    y(300, 2, 7,  "癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 己丑 戊午 戊子"),
    y(301, 1, 26, "丁巳 丁亥 丙辰 閏 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥"),
    y(302, 2, 14, "辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午"), 
    y(303, 2, 3,  "乙亥 乙巳 甲戌 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 閏 庚午"),
    y(304, 2, 22, "己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 丙申 乙丑 乙未 甲子"), 
    y(305, 2, 11, "甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午"),
    y(306, 1, 31, "戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 閏 甲申 甲寅 癸未 癸丑 壬午"), 
    y(307, 2, 19, "壬子 辛巳 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑"),
    y(308, 2, 8,  "丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 癸卯 壬申 壬寅 辛未"), 
    y(309, 1, 28, "辛丑 庚午 庚子 己巳 閏 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未"),
    y(310, 2, 16, "乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑"), 
    y(311, 2, 5,  "己未 戊子 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申"),
    y(312, 1, 25, "癸丑 閏 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 庚戌 己卯 己酉 戊寅 戊申"), 
    y(313, 2, 12, "丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅"),
    y(314, 2, 1,  "辛未 辛丑 庚午 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 閏 丁卯 丙申 丙寅"), 
    y(315, 2, 20, "乙未 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申"),
    y(316, 2, 10, "庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯"), 
    y(317, 1, 29, "甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 閏 辛亥 庚辰 庚戌 己卯 己酉 己卯"),
    y(318, 2, 17, "戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉"), 
    y(319, 2, 6,  "壬寅 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯"),
    y(320, 1, 27, "丁酉 丙寅 丙申 閏 乙丑 乙未 甲子 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯"), 
    y(321, 2, 14, "辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 丙戌"),
    y(322, 2, 3,  "乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 閏 庚辰 己酉"), 
    y(323, 2, 22, "己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰"),
    y(324, 2, 11, "癸酉 癸卯 壬申 壬寅 辛未 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌"), 
    y(325, 1, 31, "戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 閏 甲子 癸巳 癸亥 癸巳 壬戌"),
    y(326, 2, 19, "壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰"), 
    y(327, 2, 8,  "丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥"),
    y(328, 1, 28, "庚辰 庚戌 己卯 己酉 戊寅 閏 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥"), 
    y(329, 2, 15, "甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 庚子 己巳"),
    y(330, 2, 5,  "己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥"), 
    y(331, 1, 25, "癸巳 閏 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥"),
    y(332, 2, 13, "丁巳 丙戌 丙辰 乙酉 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午"), 
    y(333, 2, 1,  "辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 閏 丁未 丙子 丙午"),
    y(334, 2, 20, "乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子"), 
    y(335, 2, 10, "庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午"),
    y(336, 1, 30, "甲子 癸巳 癸亥 壬辰 壬戌 壬辰 辛酉 閏 辛卯 庚申 庚寅 己未 己丑 戊午"), 
    y(337, 2, 17, "戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 甲寅 癸未 癸丑"),
    y(338, 2, 6,  "壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未"), 
    y(339, 1, 27, "丁丑 丙午 丙子 乙巳 閏 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未"),
    y(340, 2, 14, "庚子 庚午 己亥 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑"), 
    y(341, 2, 3,  "乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 閏 己丑"),
    y(342, 2, 22, "己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 甲申"), 
    y(343, 2, 11, "癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅"),
    y(344, 1, 31, "丁未 丁丑 丙午 丙子 丙午 乙亥 乙巳 甲戌 閏 甲辰 癸酉 癸卯 壬申 壬寅"), 
    y(345, 2, 18, "辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 戊辰 丁酉 丁卯 丙申"),
    y(346, 2, 8,  "丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 辛卯"), 
    y(347, 1, 28, "庚申 庚寅 己未 己丑 戊午 閏 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅"),
    y(348, 2, 16, "甲申 癸丑 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉"), 
    y(349, 2, 4,  "戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 乙亥 甲辰 甲戌 癸卯"),
    y(350, 1, 25, "癸酉 壬寅 閏 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 戊戌 丁卯"), 
    y(351, 2, 13, "丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉"),
    y(352, 2, 2,  "辛卯 庚申 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 閏 丙戌 丙辰 乙酉"), 
    y(353, 2, 20, "乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 壬午 辛亥 辛巳 庚戌 庚辰"),
    y(354, 2, 9,  "己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 乙巳 甲戌"), 
    y(355, 1, 30, "甲辰 癸酉 癸卯 壬申 壬寅 辛未 閏 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌"),
    y(356, 2, 17, "丁卯 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰"), 
    y(357, 2, 6,  "壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 己丑 戊午 戊子 丁巳 丁亥"),
    y(358, 1, 26, "丙辰 丙戌 乙卯 閏 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 壬子 辛巳 辛亥"), 
    y(359, 2, 14, "庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳"),
    y(360, 2, 3,  "甲戌 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 閏 己巳"),
    y(361, 2, 21, "戊戌 戊辰 丁酉 丁卯 丙申 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥"),
    y(362, 2, 11, "癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 己未 戊子 戊午"), 
    y(363, 1, 31, "丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 閏 癸未 癸丑 壬午 壬子 辛巳"),
    y(364, 2, 19, "辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子"), 
    y(365, 2, 7,  "乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午"),
    y(366, 1, 28, "庚子 己巳 己亥 戊辰 閏 戊戌 丁卯 丁酉 丙寅 丙申 丙寅 乙未 乙丑 甲午"), 
    y(367, 2, 16, "甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子"),
    y(368, 2, 5,  "戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未"), 
    y(369, 1, 24, "壬子 閏 壬午 辛亥 辛巳 庚戌 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未"),
    y(370, 2, 12, "丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅 壬申 辛丑"), 
    y(371, 2, 2,  "辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 閏 丙寅 乙未 乙丑"),
    y(372, 2, 21, "乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未"), 
    y(373, 2, 9,  "己丑 戊午 戊子 丁巳 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅"),
    y(374, 1, 29, "癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 閏 庚戌 己卯 己酉 己卯 戊申 戊寅"), 
    y(375, 2, 17, "丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申"),
    y(376, 2, 7,  "壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅"), 
    y(377, 1, 26, "丙申 乙丑 乙未 閏 甲子 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅"),
    y(378, 2, 14, "庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 丙戌 乙卯 乙酉"), 
    y(379, 2, 3,  "甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 閏 己酉"),
    y(380, 2, 22, "戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯"),
    y(381, 2, 10, "壬申 壬寅 辛未 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉"),
    y(382, 1, 31, "丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 閏 癸巳 壬戌 壬辰 辛酉"), 
    y(383, 2, 19, "辛卯 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰"),
    y(384, 2, 8,  "乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌"), 
    y(385, 1, 27, "己卯 己酉 戊寅 戊申 戊寅 閏 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌"),
    y(386, 2, 15, "癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 庚子 己巳 己亥 戊辰"), 
    y(387, 2, 5,  "戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 癸亥"),
    y(388, 1, 25, "壬辰 閏 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌"), 
    y(389, 2, 12, "丙辰 乙酉 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳"),
    y(390, 2, 1,  "庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丁未 丙子 閏 丙午 乙亥 乙巳"), 
    y(391, 2, 20, "甲戌 甲辰 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 庚午 己亥"),
    y(392, 2, 10, "己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳"), 
    y(393, 1, 29, "癸亥 壬辰 壬戌 壬辰 辛酉 辛卯 庚申 閏 庚寅 己未 己丑 戊午 戊子 丁巳"),
    y(394, 2, 17, "丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 甲寅 癸未 癸丑 壬午 壬子"), 
    y(395, 2, 6,  "辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丁丑 丙午"),
    y(396, 1, 27, "丙子 乙巳 乙亥 閏 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午"), 
    y(397, 2, 13, "己亥 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子"),
    y(398, 2, 3,  "甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 辛酉 庚寅 庚申 己丑 閏 己未 戊子"), 
    y(399, 2, 22, "戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 甲申 癸丑 癸未"),
    y(400, 2, 11, "壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑"),
    y(401, 1, 30, "丙午 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 閏 癸卯 壬申 壬寅 辛未 辛丑"),
    y(402, 2, 18, "庚午 庚子 己巳 己亥 戊辰 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未"), 
    y(403, 2, 8,  "乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 辛卯 庚申 庚寅"),
    y(404, 1, 28, "己未 己丑 戊午 戊子 丁巳 閏 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑"), 
    y(405, 2, 15, "癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申"),
    y(406, 2, 4,  "丁丑 丁未 丙子 丙午 乙亥 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅"), 
    y(407, 1, 25, "壬申 辛丑 閏 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 戊戌 丁卯 丁酉 丙寅"),
    y(408, 2, 13, "丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申"), 
    y(409, 2, 1,  "庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 閏 乙酉 乙卯 甲申"),
    y(410, 2, 20, "甲寅 癸未 癸丑 壬午 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯"), 
    y(411, 2, 9,  "戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 乙巳 甲戌 甲辰 癸酉"),
    y(412, 1, 30, "癸卯 壬申 壬寅 辛未 辛丑 庚午 閏 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉"), 
    y(413, 2, 17, "丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯"),
    y(414, 2, 6,  "辛酉 庚寅 庚申 己丑 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌"), 
    y(415, 1, 26, "乙卯 乙酉 甲寅 閏 甲申 癸丑 癸未 壬子 壬午 壬子 辛巳 辛亥 庚辰 庚戌"),
    y(416, 2, 14, "己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰"), 
    y(417, 2, 3,  "甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 閏 戊辰"),
    y(418, 2, 21, "丁酉 丁卯 丙申 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌"), 
    y(419, 2, 11, "壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 己未 戊子 戊午 丁亥 丁巳"),
    y(420, 1, 31, "丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 閏 壬午 壬子 辛巳 辛亥 辛巳"), 
    y(421, 2, 18, "庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥"),
    y(422, 2, 7,  "甲辰 甲戌 癸卯 癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳"), 
    y(423, 1, 28, "己亥 戊辰 戊戌 丁卯 閏 丁酉 丙寅 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳"),
    y(424, 2, 16, "癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 戊子"), 
    y(425, 2, 4,  "丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午"),
    y(426, 1, 24, "辛亥 閏 辛巳 庚戌 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午"), 
    y(427, 2, 12, "乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子"),
    y(428, 2, 2,  "庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 閏 乙丑 乙未 甲子"), 
    y(429, 2, 20, "甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午"),
    y(430, 2, 9,  "戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑"), 
    y(431, 1, 29, "壬午 壬子 辛巳 辛亥 庚辰 庚戌 閏 庚辰 己酉 己卯 戊申 戊寅 丁未 丁丑"),
    y(432, 2, 17, "丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 壬寅 辛未"), 
    y(433, 2, 6,  "辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑"),
    y(434, 1, 26, "乙未 甲子 甲午 閏 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑"), 
    y(435, 2, 14, "己未 戊子 戊午 丁亥 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申"),
    y(436, 2, 3,  "癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 己酉 戊寅 閏 戊申"), 
    y(437, 2, 21, "丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 壬寅"),
    y(438, 2, 10, "辛未 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申"), 
    y(439, 1, 31, "丙寅 乙未 乙丑 甲午 甲子 甲午 癸亥 癸巳 壬戌 閏 壬辰 辛酉 辛卯 庚申"),
    ce440, ce441, ce442, ce443, ce444,
    y(445, 1, 24, "辛卯 辛酉 庚寅 庚申 己丑 閏 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉"),
    ce446,
    y(447, 2, 1,  "己酉 己卯 戊申 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌"),
    y(448, 1, 22, "甲辰 癸酉 閏 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 庚午 己亥 己巳 戊戌"), 
    ce449,
    y(450, 1, 29, "壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 己丑 戊午 戊子 閏 丁巳 丁亥 丙辰"),
    ce451, ce452, ce453, ce454,
    y(455, 2, 3,  "癸巳 壬戌 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午"),
    y(456, 1, 23, "丁亥 丁巳 丙戌 閏 丙辰 乙酉 乙卯 甲申 甲寅 甲申 癸丑 癸未 壬子 壬午"),
    ce457, ce458, ce459, ce460, ce461, ce462, ce463,
    y(464, 1, 25, "辛未 庚子 庚午 己亥 己巳 閏 戊戌 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑"),
    ce465, ce466, ce467, ce468,
    y(469, 1, 29, "壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 閏 丁卯 丙申"),
    ce470, ce471, 
    y(472, 1, 26, "甲寅 甲申 癸丑 癸未 壬子 壬午 壬子 閏 辛巳 辛亥 庚辰 庚戌 己卯 己酉"),
    ce473, ce474, ce475, ce476, 
    y(477, 1, 30, "乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 辛巳 庚戌 閏 庚辰"),
    ce478, ce479, ce480, ce481, ce482,
    y(483, 1, 24, "庚戌 庚辰 庚戌 己卯 己酉 閏 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳"),
    ce484, ce485, ce486, ce487,
    y(488, 1, 29, "辛巳 辛亥 庚辰 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未 閏 丁丑 丙午 丙子"),
    ce489, ce490, ce491, ce492, ce493, ce494, ce495,
    y(496, 1, 31, "乙丑 甲午 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 閏 己未"),
    ce497, ce498, ce499, ce500, ce501, ce502,
    y(503, 2, 12, ""),
    y(504, 2, 1,  ""), 
    y(505, 1, 21, ""),
    y(506, 2, 9,  ""), 
    y(507, 1, 29, ""),
    y(508, 2, 28, ""), 
    y(509, 2, 28, ""),
    y(510, 2, 28, ""), 
    y(511, 2, 28, ""),
    y(512, 2, 28, ""), 
    y(513, 2, 28, ""),
    y(514, 2, 28, ""), 
    y(515, 2, 28, ""),
    y(516, 2, 28, ""), 
    y(517, 2, 28, ""),
    y(518, 2, 28, ""), 
    y(519, 2, 28, ""),
    y(520, 2, 28, ""), 
    y(521, 2, 28, ""),
    y(522, 2, 28, ""), 
    y(523, 2, 28, ""),
    y(524, 2, 28, ""), 
    y(525, 2, 28, ""),
    y(526, 2, 28, ""), 
    y(527, 2, 28, ""),
    y(528, 2, 28, ""), 
    y(529, 2, 28, ""),
    y(530, 2, 28, ""), 
    y(531, 2, 28, ""),
    y(532, 2, 28, ""), 
    y(533, 2, 28, ""),
    y(534, 2, 28, ""), 
    y(535, 2, 28, ""),
    y(536, 2, 28, ""), 
    y(537, 2, 28, ""),
    y(538, 2, 28, ""), 
    y(539, 2, 28, ""),
    y(540, 2, 28, ""), 
    y(541, 2, 28, ""),
    y(542, 2, 28, ""), 
    y(543, 2, 28, ""),
    y(544, 2, 28, ""), 
    y(545, 2, 28, ""),
    y(546, 2, 28, ""), 
    y(547, 2, 28, ""),
    y(548, 2, 28, ""), 
    y(549, 2, 28, ""),
    y(550, 2, 28, ""), 
    y(551, 2, 28, ""),
    y(552, 2, 28, ""), 
    y(553, 2, 28, ""),
    y(554, 2, 28, ""), 
    y(555, 2, 28, ""),
    y(556, 2, 28, ""), 
    y(557, 2, 28, ""),
    y(558, 2, 28, ""), 
    y(559, 2, 28, ""),
    y(560, 2, 28, ""),
    y(561, 2, 28, ""),
    y(562, 2, 28, ""), 
    y(563, 2, 28, ""),
    y(564, 2, 28, ""), 
    y(565, 2, 28, ""),
    y(566, 2, 28, ""), 
    y(567, 2, 28, ""),
    y(568, 2, 28, ""), 
    y(569, 2, 28, ""),
    y(570, 2, 28, ""), 
    y(571, 2, 28, ""),
    y(572, 2, 28, ""), 
    y(573, 2, 28, ""),
    y(574, 2, 28, ""), 
    y(575, 2, 28, ""),
    y(576, 2, 28, ""), 
    y(577, 2, 28, ""),
    y(578, 2, 28, ""), 
    y(579, 2, 28, ""),
    y(580, 2, 28, ""),
    y(581, 2, 28, ""),
    y(582, 2, 28, ""), 
    y(583, 2, 28, ""),
    y(584, 2, 28, ""), 
    y(585, 2, 28, ""),
    y(586, 2, 28, ""), 
    y(587, 2, 28, ""),
    y(588, 2, 28, ""), 
    y(589, 2, 28, ""),
    y(590, 2, 28, ""), 
    y(591, 2, 28, ""),
    y(592, 2, 28, ""), 
    y(593, 2, 28, ""),
    y(594, 2, 28, ""), 
    y(595, 2, 28, ""),
    y(596, 2, 28, ""), 
    y(597, 2, 28, ""),
    y(598, 2, 28, ""), 
    y(599, 2, 28, ""),
    y(600, 2, 28, ""),
    y(601, 2, 28, ""),
    y(602, 2, 28, ""), 
    y(603, 2, 28, ""),
    y(604, 2, 28, ""), 
    y(605, 2, 28, ""),
    y(606, 2, 28, ""), 
    y(607, 2, 28, ""),
    y(608, 2, 28, ""), 
    y(609, 2, 28, ""),
    y(610, 2, 28, ""), 
    y(611, 2, 28, ""),
    y(612, 2, 28, ""), 
    y(613, 2, 28, ""),
    y(614, 2, 28, ""), 
    y(615, 2, 28, ""),
    y(616, 2, 28, ""), 
    y(617, 2, 28, ""),
    y(618, 2, 28, ""), 
    y(619, 2, 28, ""),
    y(620, 2, 28, ""), 
    y(621, 2, 28, ""),
    y(622, 2, 28, ""), 
    y(623, 2, 28, ""),
    y(624, 2, 28, ""), 
    y(625, 2, 28, ""),
    y(626, 2, 28, ""), 
    y(627, 2, 28, ""),
    y(628, 2, 28, ""), 
    y(629, 2, 28, ""),
    y(630, 2, 28, ""), 
    y(631, 2, 28, ""),
    y(632, 2, 28, ""), 
    y(633, 2, 28, ""),
    y(634, 2, 28, ""), 
    y(635, 2, 28, ""),
    y(636, 2, 28, ""), 
    y(637, 2, 28, ""),
    y(638, 2, 28, ""), 
    y(639, 2, 28, ""),
    y(640, 2, 28, ""), 
    y(641, 2, 28, ""),
    y(642, 2, 28, ""), 
    y(643, 2, 28, ""),
    y(644, 2, 28, ""), 
    y(645, 2, 28, ""),
    y(646, 2, 28, ""), 
    y(647, 2, 28, ""),
    y(648, 2, 28, ""), 
    y(649, 2, 28, ""),
    y(650, 2, 28, ""), 
    y(651, 2, 28, ""),
    y(652, 2, 28, ""), 
    y(653, 2, 28, ""),
    y(654, 2, 28, ""), 
    y(655, 2, 28, ""),
    y(656, 2, 28, ""), 
    y(657, 2, 28, ""),
    y(658, 2, 28, ""), 
    y(659, 2, 28, ""),
    y(660, 2, 28, ""),
    y(661, 2, 28, ""),
    y(662, 2, 28, ""), 
    y(663, 2, 28, ""),
    y(664, 2, 28, ""), 
    y(665, 2, 28, ""),
    y(666, 2, 28, ""), 
    y(667, 2, 28, ""),
    y(668, 2, 28, ""), 
    y(669, 2, 28, ""),
    y(670, 2, 28, ""), 
    y(671, 2, 28, ""),
    y(672, 2, 28, ""), 
    y(673, 2, 28, ""),
    y(674, 2, 28, ""), 
    y(675, 2, 28, ""),
    y(676, 2, 28, ""), 
    y(677, 2, 28, ""),
    y(678, 2, 28, ""), 
    y(679, 2, 28, ""),
    y(680, 2, 28, ""),
    y(681, 2, 28, ""),
    y(682, 2, 28, ""), 
    y(683, 2, 28, ""),
    y(684, 2, 28, ""), 
    y(685, 2, 28, ""),
    y(686, 2, 28, ""), 
    y(687, 2, 28, ""),
    y(688, 2, 28, ""), 
    y(689, 2, 28, ""),
    y(690, 2, 28, ""), 
    y(691, 2, 28, ""),
    y(692, 2, 28, ""), 
    y(693, 2, 28, ""),
    y(694, 2, 28, ""), 
    y(695, 2, 28, ""),
    y(696, 2, 28, ""), 
    y(697, 2, 28, ""),
    y(698, 2, 28, ""), 
    y(699, 2, 28, ""),    
    y(700, 2, 28, ""),
    y(701, 2, 28, ""),
    y(702, 2, 28, ""), 
    y(703, 2, 28, ""),
    y(704, 2, 28, ""), 
    y(705, 2, 28, ""),
    y(706, 2, 28, ""), 
    y(707, 2, 28, ""),
    y(708, 2, 28, ""), 
    y(709, 2, 28, ""),
    y(710, 2, 28, ""), 
    y(711, 2, 28, ""),
    y(712, 2, 28, ""), 
    y(713, 2, 28, ""),
    y(714, 2, 28, ""), 
    y(715, 2, 28, ""),
    y(716, 2, 28, ""), 
    y(717, 2, 28, ""),
    y(718, 2, 28, ""), 
    y(719, 2, 28, ""),
    y(720, 2, 28, ""), 
    y(721, 2, 28, ""),
    y(722, 2, 28, ""), 
    y(723, 2, 28, ""),
    y(724, 2, 28, ""), 
    y(725, 2, 28, ""),
    y(726, 2, 28, ""), 
    y(727, 2, 28, ""),
    y(728, 2, 28, ""), 
    y(729, 2, 28, ""),
    y(730, 2, 28, ""), 
    y(731, 2, 28, ""),
    y(732, 2, 28, ""), 
    y(733, 2, 28, ""),
    y(734, 2, 28, ""), 
    y(735, 2, 28, ""),
    y(736, 2, 28, ""), 
    y(737, 2, 28, ""),
    y(738, 2, 28, ""), 
    y(739, 2, 28, ""),
    y(740, 2, 28, ""), 
    y(741, 2, 28, ""),
    y(742, 2, 28, ""), 
    y(743, 2, 28, ""),
    y(744, 2, 28, ""), 
    y(745, 2, 28, ""),
    y(746, 2, 28, ""), 
    y(747, 2, 28, ""),
    y(748, 2, 28, ""), 
    y(749, 2, 28, ""),
    y(750, 2, 28, ""), 
    y(751, 2, 28, ""),
    y(752, 2, 28, ""), 
    y(753, 2, 28, ""),
    y(754, 2, 28, ""), 
    y(755, 2, 28, ""),
    y(756, 2, 28, ""), 
    y(757, 2, 28, ""),
    y(758, 2, 28, ""), 
    y(759, 2, 28, ""),
    y(760, 2, 28, ""),
    y(761, 2, 28, ""),
    y(762, 2, 28, ""), 
    y(763, 2, 28, ""),
    y(764, 2, 28, ""), 
    y(765, 2, 28, ""),
    y(766, 2, 28, ""), 
    y(767, 2, 28, ""),
    y(768, 2, 28, ""), 
    y(769, 2, 28, ""),
    y(770, 2, 28, ""), 
    y(771, 2, 28, ""),
    y(772, 2, 28, ""), 
    y(773, 2, 28, ""),
    y(774, 2, 28, ""), 
    y(775, 2, 28, ""),
    y(776, 2, 28, ""), 
    y(777, 2, 28, ""),
    y(778, 2, 28, ""), 
    y(779, 2, 28, ""),
    y(780, 2, 28, ""),
    y(781, 2, 28, ""),
    y(782, 2, 28, ""), 
    y(783, 2, 28, ""),
    y(784, 2, 28, ""), 
    y(785, 2, 28, ""),
    y(786, 2, 28, ""), 
    y(787, 2, 28, ""),
    y(788, 2, 28, ""), 
    y(789, 2, 28, ""),
    y(790, 2, 28, ""), 
    y(791, 2, 28, ""),
    y(792, 2, 28, ""), 
    y(793, 2, 28, ""),
    y(794, 2, 28, ""), 
    y(795, 2, 28, ""),
    y(796, 2, 28, ""), 
    y(797, 2, 28, ""),
    y(798, 2, 28, ""), 
    y(799, 2, 28, ""),
    y(800, 2, 28, ""),
    y(801, 2, 28, ""),
    y(802, 2, 28, ""), 
    y(803, 2, 28, ""),
    y(804, 2, 28, ""), 
    y(805, 2, 28, ""),
    y(806, 2, 28, ""), 
    y(807, 2, 28, ""),
    y(808, 2, 28, ""), 
    y(809, 2, 28, ""),
    y(810, 2, 28, ""), 
    y(811, 2, 28, ""),
    y(812, 2, 28, ""), 
    y(813, 2, 28, ""),
    y(814, 2, 28, ""), 
    y(815, 2, 28, ""),
    y(816, 2, 28, ""), 
    y(817, 2, 28, ""),
    y(818, 2, 28, ""), 
    y(819, 2, 28, ""),
    y(820, 2, 28, ""), 
    y(821, 2, 28, ""),
    y(822, 2, 28, ""), 
    y(823, 2, 28, ""),
    y(824, 2, 28, ""), 
    y(825, 2, 28, ""),
    y(826, 2, 28, ""), 
    y(827, 2, 28, ""),
    y(828, 2, 28, ""), 
    y(829, 2, 28, ""),
    y(830, 2, 28, ""), 
    y(831, 2, 28, ""),
    y(832, 2, 28, ""), 
    y(833, 2, 28, ""),
    y(834, 2, 28, ""), 
    y(835, 2, 28, ""),
    y(836, 2, 28, ""), 
    y(837, 2, 28, ""),
    y(838, 2, 28, ""), 
    y(839, 2, 28, ""),
    y(840, 2, 28, ""), 
    y(841, 2, 28, ""),
    y(842, 2, 28, ""), 
    y(843, 2, 28, ""),
    y(844, 2, 28, ""), 
    y(845, 2, 28, ""),
    y(846, 2, 28, ""), 
    y(847, 2, 28, ""),
    y(848, 2, 28, ""), 
    y(849, 2, 28, ""),
    y(850, 2, 28, ""), 
    y(851, 2, 28, ""),
    y(852, 2, 28, ""), 
    y(853, 2, 28, ""),
    y(854, 2, 28, ""), 
    y(855, 2, 28, ""),
    y(856, 2, 28, ""), 
    y(857, 2, 28, ""),
    y(858, 2, 28, ""), 
    y(859, 2, 28, ""),
    y(860, 2, 28, ""),
    y(861, 2, 28, ""),
    y(862, 2, 28, ""), 
    y(863, 2, 28, ""),
    y(864, 2, 28, ""), 
    y(865, 2, 28, ""),
    y(866, 2, 28, ""), 
    y(867, 2, 28, ""),
    y(868, 2, 28, ""), 
    y(869, 2, 28, ""),
    y(870, 2, 28, ""), 
    y(871, 2, 28, ""),
    y(872, 2, 28, ""), 
    y(873, 2, 28, ""),
    y(874, 2, 28, ""), 
    y(875, 2, 28, ""),
    y(876, 2, 28, ""), 
    y(877, 2, 28, ""),
    y(878, 2, 28, ""), 
    y(879, 2, 28, ""),
    y(880, 2, 28, ""),
    y(881, 2, 28, ""),
    y(882, 2, 28, ""), 
    y(883, 2, 28, ""),
    y(884, 2, 28, ""), 
    y(885, 2, 28, ""),
    y(886, 2, 28, ""), 
    y(887, 2, 28, ""),
    y(888, 2, 28, ""), 
    y(889, 2, 28, ""),
    y(890, 2, 28, ""), 
    y(891, 2, 28, ""),
    y(892, 2, 28, ""), 
    y(893, 2, 28, ""),
    y(894, 2, 28, ""), 
    y(895, 2, 28, ""),
    y(896, 2, 28, ""), 
    y(897, 2, 28, ""),
    y(898, 2, 28, ""), 
    y(899, 2, 28, ""),
    y(900, 2, 28, ""),
    y(901, 2, 28, ""),
    y(902, 2, 28, ""), 
    y(903, 2, 28, ""),
    y(904, 2, 28, ""), 
    y(905, 2, 28, ""),
    y(906, 2, 28, ""), 
    y(907, 2, 28, ""),
    y(908, 2, 28, ""), 
    y(909, 2, 28, ""),
    y(910, 2, 28, ""), 
    y(911, 2, 28, ""),
    y(912, 2, 28, ""), 
    y(913, 2, 28, ""),
    y(914, 2, 28, ""), 
    y(915, 2, 28, ""),
    y(916, 2, 28, ""), 
    y(917, 2, 28, ""),
    y(918, 2, 28, ""), 
    y(919, 2, 28, ""),
    y(920, 2, 28, ""), 
    y(921, 2, 28, ""),
    y(922, 2, 28, ""), 
    y(923, 2, 28, ""),
    y(924, 2, 28, ""), 
    y(925, 2, 28, ""),
    y(926, 2, 28, ""), 
    y(927, 2, 28, ""),
    y(928, 2, 28, ""), 
    y(929, 2, 28, ""),
    y(930, 2, 28, ""), 
    y(931, 2, 28, ""),
    y(932, 2, 28, ""), 
    y(933, 2, 28, ""),
    y(934, 2, 28, ""), 
    y(935, 2, 28, ""),
    y(936, 2, 28, ""), 
    y(937, 2, 28, ""),
    y(938, 2, 28, ""), 
    y(939, 2, 28, ""),
    y(940, 2, 28, ""), 
    y(941, 2, 28, ""),
    y(942, 2, 28, ""), 
    y(943, 2, 28, ""),
    y(944, 2, 28, ""), 
    y(945, 2, 28, ""),
    y(946, 2, 28, ""), 
    y(947, 2, 28, ""),
    y(948, 2, 28, ""), 
    y(949, 2, 28, ""),
    y(950, 2, 28, ""), 
    y(951, 2, 28, ""),
    y(952, 2, 28, ""), 
    y(953, 2, 28, ""),
    y(954, 2, 28, ""), 
    y(955, 2, 28, ""),
    y(956, 2, 28, ""), 
    y(957, 2, 28, ""),
    y(958, 2, 28, ""), 
    y(959, 2, 28, ""),
    y(960, 2, 28, ""),
    y(961, 2, 28, ""),
    y(962, 2, 28, ""), 
    y(963, 2, 28, ""),
    y(964, 2, 28, ""), 
    y(965, 2, 28, ""),
    y(966, 2, 28, ""), 
    y(967, 2, 28, ""),
    y(968, 2, 28, ""), 
    y(969, 2, 28, ""),
    y(970, 2, 28, ""), 
    y(971, 2, 28, ""),
    y(972, 2, 28, ""), 
    y(973, 2, 28, ""),
    y(974, 2, 28, ""), 
    y(975, 2, 28, ""),
    y(976, 2, 28, ""), 
    y(977, 2, 28, ""),
    y(978, 2, 28, ""), 
    y(979, 2, 28, ""),
    y(980, 2, 28, ""),
    y(981, 2, 28, ""),
    y(982, 2, 28, ""), 
    y(983, 2, 28, ""),
    y(984, 2, 28, ""), 
    y(985, 2, 28, ""),
    y(986, 2, 28, ""), 
    y(987, 2, 28, ""),
    y(988, 2, 28, ""), 
    y(989, 2, 28, ""),
    y(990, 2, 28, ""), 
    y(991, 2, 28, ""),
    y(992, 2, 28, ""), 
    y(993, 2, 28, ""),
    y(994, 2, 28, ""), 
    y(995, 2, 28, ""),
    y(996, 2, 28, ""), 
    y(997, 2, 28, ""),
    y(998, 2, 28, ""), 
    y(999, 2, 28, ""),
    y(1000, 2, 28, ""),
    y(1001, 2, 28, ""),
    y(1002, 2, 28, ""), 
    y(1003, 2, 28, ""),
    y(1004, 2, 28, ""), 
    y(1005, 2, 28, ""),
    y(1006, 2, 28, ""), 
    y(1007, 2, 28, ""),
    y(1008, 2, 28, ""), 
    y(1009, 2, 28, ""),
    y(1010, 2, 28, ""), 
    y(1011, 2, 28, ""),
    y(1012, 2, 28, ""), 
    y(1013, 2, 28, ""),
    y(1014, 2, 28, ""), 
    y(1015, 2, 28, ""),
    y(1016, 2, 28, ""), 
    y(1017, 2, 28, ""),
    y(1018, 2, 28, ""), 
    y(1019, 2, 28, ""),
    y(1020, 2, 28, ""), 
    y(1021, 2, 28, ""),
    y(1022, 2, 28, ""), 
    y(1023, 2, 28, ""),
    y(1024, 2, 28, ""), 
    y(1025, 2, 28, ""),
    y(1026, 2, 28, ""), 
    y(1027, 2, 28, ""),
    y(1028, 2, 28, ""), 
    y(1029, 2, 28, ""),
    y(1030, 2, 28, ""), 
    y(1031, 2, 28, ""),
    y(1032, 2, 28, ""), 
    y(1033, 2, 28, ""),
    y(1034, 2, 28, ""), 
    y(1035, 2, 28, ""),
    y(1036, 2, 28, ""), 
    y(1037, 2, 28, ""),
    y(1038, 2, 28, ""), 
    y(1039, 2, 28, ""),
    y(1040, 2, 28, ""), 
    y(1041, 2, 28, ""),
    y(1042, 2, 28, ""), 
    y(1043, 2, 28, ""),
    y(1044, 2, 28, ""), 
    y(1045, 2, 28, ""),
    y(1046, 2, 28, ""), 
    y(1047, 2, 28, ""),
    y(1048, 2, 28, ""), 
    y(1049, 2, 28, ""),
    y(1050, 2, 28, ""), 
    y(1051, 2, 28, ""),
    y(1052, 2, 28, ""), 
    y(1053, 2, 28, ""),
    y(1054, 2, 28, ""), 
    y(1055, 2, 28, ""),
    y(1056, 2, 28, ""), 
    y(1057, 2, 28, ""),
    y(1058, 2, 28, ""), 
    y(1059, 2, 28, ""),
    y(1060, 2, 28, ""),
    y(1061, 2, 28, ""),
    y(1062, 2, 28, ""), 
    y(1063, 2, 28, ""),
    y(1064, 2, 28, ""), 
    y(1065, 2, 28, ""),
    y(1066, 2, 28, ""), 
    y(1067, 2, 28, ""),
    y(1068, 2, 28, ""), 
    y(1069, 2, 28, ""),
    y(1070, 2, 28, ""), 
    y(1071, 2, 28, ""),
    y(1072, 2, 28, ""), 
    y(1073, 2, 28, ""),
    y(1074, 2, 28, ""), 
    y(1075, 2, 28, ""),
    y(1076, 2, 28, ""), 
    y(1077, 2, 28, ""),
    y(1078, 2, 28, ""), 
    y(1079, 2, 28, ""),
    y(1080, 2, 28, ""),
    y(1081, 2, 28, ""),
    y(1082, 2, 28, ""), 
    y(1083, 2, 28, ""),
    y(1084, 2, 28, ""), 
    y(1085, 2, 28, ""),
    y(1086, 2, 28, ""), 
    y(1087, 2, 28, ""),
    y(1088, 2, 28, ""), 
    y(1089, 2, 28, ""),
    y(1090, 2, 28, ""), 
    y(1091, 2, 28, ""),
    y(1092, 2, 28, ""), 
    y(1093, 2, 28, ""),
    y(1094, 2, 28, ""), 
    y(1095, 2, 28, ""),
    y(1096, 2, 28, ""), 
    y(1097, 2, 28, ""),
    y(1098, 2, 28, ""), 
    y(1099, 2, 28, ""),
    y(1100, 2, 28, ""),
    y(1101, 2, 28, ""),
    y(1102, 2, 28, ""), 
    y(1103, 2, 28, ""),
    y(1104, 2, 28, ""), 
    y(1105, 2, 28, ""),
    y(1106, 2, 28, ""), 
    y(1107, 2, 28, ""),
    y(1108, 2, 28, ""), 
    y(1109, 2, 28, ""),
    y(1110, 2, 28, ""), 
    y(1111, 2, 28, ""),
    y(1112, 2, 28, ""), 
    y(1113, 2, 28, ""),
    y(1114, 2, 28, ""), 
    y(1115, 2, 28, ""),
    y(1116, 2, 28, ""), 
    y(1117, 2, 28, ""),
    y(1118, 2, 28, ""), 
    y(1119, 2, 28, ""),
    y(1120, 2, 28, ""), 
    y(1121, 2, 28, ""),
    y(1122, 2, 28, ""), 
    y(1123, 2, 28, ""),
    y(1124, 2, 28, ""), 
    y(1125, 2, 28, ""),
    y(1126, 2, 28, ""), 
    y(1127, 2, 28, ""),
    y(1128, 2, 28, ""), 
    y(1129, 2, 28, ""),
    y(1130, 2, 28, ""), 
    y(1131, 2, 28, ""),
    y(1132, 2, 28, ""), 
    y(1133, 2, 28, ""),
    y(1134, 2, 28, ""), 
    y(1135, 2, 28, ""),
    y(1136, 2, 28, ""), 
    y(1137, 2, 28, ""),
    y(1138, 2, 28, ""), 
    y(1139, 2, 28, ""),
    y(1140, 2, 28, ""), 
    y(1141, 2, 28, ""),
    y(1142, 2, 28, ""), 
    y(1143, 2, 28, ""),
    y(1144, 2, 28, ""), 
    y(1145, 2, 28, ""),
    y(1146, 2, 28, ""), 
    y(1147, 2, 28, ""),
    y(1148, 2, 28, ""), 
    y(1149, 2, 28, ""),
    y(1150, 2, 28, ""), 
    y(1151, 2, 28, ""),
    y(1152, 2, 28, ""), 
    y(1153, 2, 28, ""),
    y(1154, 2, 28, ""), 
    y(1155, 2, 28, ""),
    y(1156, 2, 28, ""), 
    y(1157, 2, 28, ""),
    y(1158, 2, 28, ""), 
    y(1159, 2, 28, ""),
    y(1160, 2, 28, ""),
    y(1161, 2, 28, ""),
    y(1162, 2, 28, ""), 
    y(1163, 2, 28, ""),
    y(1164, 2, 28, ""), 
    y(1165, 2, 28, ""),
    y(1166, 2, 28, ""), 
    y(1167, 2, 28, ""),
    y(1168, 2, 28, ""), 
    y(1169, 2, 28, ""),
    y(1170, 2, 28, ""), 
    y(1171, 2, 28, ""),
    y(1172, 2, 28, ""), 
    y(1173, 2, 28, ""),
    y(1174, 2, 28, ""), 
    y(1175, 2, 28, ""),
    y(1176, 2, 28, ""), 
    y(1177, 2, 28, ""),
    y(1178, 2, 28, ""), 
    y(1179, 2, 28, ""),
    y(1180, 2, 28, ""),
    y(1181, 2, 28, ""),
    y(1182, 2, 28, ""), 
    y(1183, 2, 28, ""),
    y(1184, 2, 28, ""), 
    y(1185, 2, 28, ""),
    y(1186, 2, 28, ""), 
    y(1187, 2, 28, ""),
    y(1188, 2, 28, ""), 
    y(1189, 2, 28, ""),
    y(1190, 2, 28, ""), 
    y(1191, 2, 28, ""),
    y(1192, 2, 28, ""), 
    y(1193, 2, 28, ""),
    y(1194, 2, 28, ""), 
    y(1195, 2, 28, ""),
    y(1196, 2, 28, ""), 
    y(1197, 2, 28, ""),
    y(1198, 2, 28, ""), 
    y(1199, 2, 28, ""),
    y(1200, 2, 28, ""),
    y(1201, 2, 28, ""),
    y(1202, 2, 28, ""), 
    y(1203, 2, 28, ""),
    y(1204, 2, 28, ""), 
    y(1205, 2, 28, ""),
    y(1206, 2, 28, ""), 
    y(1207, 2, 28, ""),
    y(1208, 2, 28, ""), 
    y(1209, 2, 28, ""),
    y(1210, 2, 28, ""), 
    y(1211, 2, 28, ""),
    y(1212, 2, 28, ""), 
    y(1213, 2, 28, ""),
    y(1214, 2, 28, ""), 
    y(1215, 2, 28, ""),
    y(1216, 2, 28, ""), 
    y(1217, 2, 28, ""),
    y(1218, 2, 28, ""), 
    y(1219, 2, 28, ""),
    y(1220, 2, 28, ""), 
    y(1221, 2, 28, ""),
    y(1222, 2, 28, ""), 
    y(1223, 2, 28, ""),
    y(1224, 2, 28, ""), 
    y(1225, 2, 28, ""),
    y(1226, 2, 28, ""), 
    y(1227, 2, 28, ""),
    y(1228, 2, 28, ""), 
    y(1229, 2, 28, ""),
    y(1230, 2, 28, ""), 
    y(1231, 2, 28, ""),
    y(1232, 2, 28, ""), 
    y(1233, 2, 28, ""),
    y(1234, 2, 28, ""), 
    y(1235, 2, 28, ""),
    y(1236, 2, 28, ""), 
    y(1237, 2, 28, ""),
    y(1238, 2, 28, ""), 
    y(1239, 2, 28, ""),
    y(1240, 2, 28, ""), 
    y(1241, 2, 28, ""),
    y(1242, 2, 28, ""), 
    y(1243, 2, 28, ""),
    y(1244, 2, 28, ""), 
    y(1245, 2, 28, ""),
    y(1246, 2, 28, ""), 
    y(1247, 2, 28, ""),
    y(1248, 2, 28, ""), 
    y(1249, 2, 28, ""),
    y(1250, 2, 28, ""), 
    y(1251, 2, 28, ""),
    y(1252, 2, 28, ""), 
    y(1253, 2, 28, ""),
    y(1254, 2, 28, ""), 
    y(1255, 2, 28, ""),
    y(1256, 2, 28, ""), 
    y(1257, 2, 28, ""),
    y(1258, 2, 28, ""), 
    y(1259, 2, 28, ""),
    y(1260, 2, 28, ""),
    y(1261, 2, 28, ""),
    y(1262, 2, 28, ""), 
    y(1263, 2, 28, ""),
    y(1264, 2, 28, ""), 
    y(1265, 2, 28, ""),
    y(1266, 2, 28, ""), 
    y(1267, 2, 28, ""),
    y(1268, 2, 28, ""), 
    y(1269, 2, 28, ""),
    y(1270, 2, 28, ""), 
    y(1271, 2, 28, ""),
    y(1272, 2, 28, ""), 
    y(1273, 2, 28, ""),
    y(1274, 2, 28, ""), 
    y(1275, 2, 28, ""),
    y(1276, 2, 28, ""), 
    y(1277, 2, 28, ""),
    y(1278, 2, 28, ""), 
    y(1279, 2, 28, ""),
    y(1280, 2, 28, ""),
    y(1281, 2, 28, ""),
    y(1282, 2, 28, ""), 
    y(1283, 2, 28, ""),
    y(1284, 2, 28, ""), 
    y(1285, 2, 28, ""),
    y(1286, 2, 28, ""), 
    y(1287, 2, 28, ""),
    y(1288, 2, 28, ""), 
    y(1289, 2, 28, ""),
    y(1290, 2, 28, ""), 
    y(1291, 2, 28, ""),
    y(1292, 2, 28, ""), 
    y(1293, 2, 28, ""),
    y(1294, 2, 28, ""), 
    y(1295, 2, 28, ""),
    y(1296, 2, 28, ""), 
    y(1297, 2, 28, ""),
    y(1298, 2, 28, ""), 
    y(1299, 2, 28, ""),
    y(1300, 2, 28, ""),
    y(1301, 2, 28, ""),
    y(1302, 2, 28, ""), 
    y(1303, 2, 28, ""),
    y(1304, 2, 28, ""), 
    y(1305, 2, 28, ""),
    y(1306, 2, 28, ""), 
    y(1307, 2, 28, ""),
    y(1308, 2, 28, ""), 
    y(1309, 2, 28, ""),
    y(1310, 2, 28, ""), 
    y(1311, 2, 28, ""),
    y(1312, 2, 28, ""), 
    y(1313, 2, 28, ""),
    y(1314, 2, 28, ""), 
    y(1315, 2, 28, ""),
    y(1316, 2, 28, ""), 
    y(1317, 2, 28, ""),
    y(1318, 2, 28, ""), 
    y(1319, 2, 28, ""),
    y(1320, 2, 28, ""), 
    y(1321, 2, 28, ""),
    y(1322, 2, 28, ""), 
    y(1323, 2, 28, ""),
    y(1324, 2, 28, ""), 
    y(1325, 2, 28, ""),
    y(1326, 2, 28, ""), 
    y(1327, 2, 28, ""),
    y(1328, 2, 28, ""), 
    y(1329, 2, 28, ""),
    y(1330, 2, 28, ""), 
    y(1331, 2, 28, ""),
    y(1332, 2, 28, ""), 
    y(1333, 2, 28, ""),
    y(1334, 2, 28, ""), 
    y(1335, 2, 28, ""),
    y(1336, 2, 28, ""), 
    y(1337, 2, 28, ""),
    y(1338, 2, 28, ""), 
    y(1339, 2, 28, ""),
    y(1340, 2, 28, ""), 
    y(1341, 2, 28, ""),
    y(1342, 2, 28, ""), 
    y(1343, 2, 28, ""),
    y(1344, 2, 28, ""), 
    y(1345, 2, 28, ""),
    y(1346, 2, 28, ""), 
    y(1347, 2, 28, ""),
    y(1348, 2, 28, ""), 
    y(1349, 2, 28, ""),
    y(1350, 2, 28, ""), 
    y(1351, 2, 28, ""),
    y(1352, 2, 28, ""), 
    y(1353, 2, 28, ""),
    y(1354, 2, 28, ""), 
    y(1355, 2, 28, ""),
    y(1356, 2, 28, ""), 
    y(1357, 2, 28, ""),
    y(1358, 2, 28, ""), 
    y(1359, 2, 28, ""),
    y(1360, 2, 28, ""),
    y(1361, 2, 28, ""),
    y(1362, 2, 28, ""), 
    y(1363, 2, 28, ""),
    y(1364, 2, 28, ""), 
    y(1365, 2, 28, ""),
    y(1366, 2, 28, ""), 
    y(1367, 2, 28, ""),
    y(1368, 2, 28, ""), 
    y(1369, 2, 28, ""),
    y(1370, 2, 28, ""), 
    y(1371, 2, 28, ""),
    y(1372, 2, 28, ""), 
    y(1373, 2, 28, ""),
    y(1374, 2, 28, ""), 
    y(1375, 2, 28, ""),
    y(1376, 2, 28, ""), 
    y(1377, 2, 28, ""),
    y(1378, 2, 28, ""), 
    y(1379, 2, 28, ""),
    y(1380, 2, 28, ""),
    y(1381, 2, 28, ""),
    y(1382, 2, 28, ""), 
    y(1383, 2, 28, ""),
    y(1384, 2, 28, ""), 
    y(1385, 2, 28, ""),
    y(1386, 2, 28, ""), 
    y(1387, 2, 28, ""),
    y(1388, 2, 28, ""), 
    y(1389, 2, 28, ""),
    y(1390, 2, 28, ""), 
    y(1391, 2, 28, ""),
    y(1392, 2, 28, ""), 
    y(1393, 2, 28, ""),
    y(1394, 2, 28, ""), 
    y(1395, 2, 28, ""),
    y(1396, 2, 28, ""), 
    y(1397, 2, 28, ""),
    y(1398, 2, 28, ""), 
    y(1399, 2, 28, ""),
    y(1400, 2, 28, ""),
    y(1401, 2, 28, ""),
    y(1402, 2, 28, ""), 
    y(1403, 2, 28, ""),
    y(1404, 2, 28, ""), 
    y(1405, 2, 28, ""),
    y(1406, 2, 28, ""), 
    y(1407, 2, 28, ""),
    y(1408, 2, 28, ""), 
    y(1409, 2, 28, ""),
    y(1410, 2, 28, ""), 
    y(1411, 2, 28, ""),
    y(1412, 2, 28, ""), 
    y(1413, 2, 28, ""),
    y(1414, 2, 28, ""), 
    y(1415, 2, 28, ""),
    y(1416, 2, 28, ""), 
    y(1417, 2, 28, ""),
    y(1418, 2, 28, ""), 
    y(1419, 2, 28, ""),
    y(1420, 2, 28, ""), 
    y(1421, 2, 28, ""),
    y(1422, 2, 28, ""), 
    y(1423, 2, 28, ""),
    y(1424, 2, 28, ""), 
    y(1425, 2, 28, ""),
    y(1426, 2, 28, ""), 
    y(1427, 2, 28, ""),
    y(1428, 2, 28, ""), 
    y(1429, 2, 28, ""),
    y(1430, 2, 28, ""), 
    y(1431, 2, 28, ""),
    y(1432, 2, 28, ""), 
    y(1433, 2, 28, ""),
    y(1434, 2, 28, ""), 
    y(1435, 2, 28, ""),
    y(1436, 2, 28, ""), 
    y(1437, 2, 28, ""),
    y(1438, 2, 28, ""), 
    y(1439, 2, 28, ""),
    y(1440, 2, 28, ""), 
    y(1441, 2, 28, ""),
    y(1442, 2, 28, ""), 
    y(1443, 2, 28, ""),
    y(1444, 2, 28, ""), 
    y(1445, 2, 28, ""),
    y(1446, 2, 28, ""), 
    y(1447, 2, 28, ""),
    y(1448, 2, 28, ""), 
    y(1449, 2, 28, ""),
    y(1450, 2, 28, ""), 
    y(1451, 2, 28, ""),
    y(1452, 2, 28, ""), 
    y(1453, 2, 28, ""),
    y(1454, 2, 28, ""), 
    y(1455, 2, 28, ""),
    y(1456, 2, 28, ""), 
    y(1457, 2, 28, ""),
    y(1458, 2, 28, ""), 
    y(1459, 2, 28, ""),
    y(1460, 2, 28, ""),
    y(1461, 2, 28, ""),
    y(1462, 2, 28, ""), 
    y(1463, 2, 28, ""),
    y(1464, 2, 28, ""), 
    y(1465, 2, 28, ""),
    y(1466, 2, 28, ""), 
    y(1467, 2, 28, ""),
    y(1468, 2, 28, ""), 
    y(1469, 2, 28, ""),
    y(1470, 2, 28, ""), 
    y(1471, 2, 28, ""),
    y(1472, 2, 28, ""), 
    y(1473, 2, 28, ""),
    y(1474, 2, 28, ""), 
    y(1475, 2, 28, ""),
    y(1476, 2, 28, ""), 
    y(1477, 2, 28, ""),
    y(1478, 2, 28, ""), 
    y(1479, 2, 28, ""),
    y(1480, 2, 28, ""),
    y(1481, 2, 28, ""),
    y(1482, 2, 28, ""), 
    y(1483, 2, 28, ""),
    y(1484, 2, 28, ""), 
    y(1485, 2, 28, ""),
    y(1486, 2, 28, ""), 
    y(1487, 2, 28, ""),
    y(1488, 2, 28, ""), 
    y(1489, 2, 28, ""),
    y(1490, 2, 28, ""), 
    y(1491, 2, 28, ""),
    y(1492, 2, 28, ""), 
    y(1493, 2, 28, ""),
    y(1494, 2, 28, ""), 
    y(1495, 2, 28, ""),
    y(1496, 2, 28, ""), 
    y(1497, 2, 28, ""),
    y(1498, 2, 28, ""), 
    y(1499, 2, 28, ""),    
    y(1500, 2, 28, ""),
    y(1501, 2, 28, ""),
    y(1502, 2, 28, ""), 
    y(1503, 2, 28, ""),
    y(1504, 2, 28, ""), 
    y(1505, 2, 28, ""),
    y(1506, 2, 28, ""), 
    y(1507, 2, 28, ""),
    y(1508, 2, 28, ""), 
    y(1509, 2, 28, ""),
    y(1510, 2, 28, ""), 
    y(1511, 2, 28, ""),
    y(1512, 2, 28, ""), 
    y(1513, 2, 28, ""),
    y(1514, 2, 28, ""), 
    y(1515, 2, 28, ""),
    y(1516, 2, 28, ""), 
    y(1517, 2, 28, ""),
    y(1518, 2, 28, ""), 
    y(1519, 2, 28, ""),
    y(1520, 2, 28, ""), 
    y(1521, 2, 28, ""),
    y(1522, 2, 28, ""), 
    y(1523, 2, 28, ""),
    y(1524, 2, 28, ""), 
    y(1525, 2, 28, ""),
    y(1526, 2, 28, ""), 
    y(1527, 2, 28, ""),
    y(1528, 2, 28, ""), 
    y(1529, 2, 28, ""),
    y(1530, 2, 28, ""), 
    y(1531, 2, 28, ""),
    y(1532, 2, 28, ""), 
    y(1533, 2, 28, ""),
    y(1534, 2, 28, ""), 
    y(1535, 2, 28, ""),
    y(1536, 2, 28, ""), 
    y(1537, 2, 28, ""),
    y(1538, 2, 28, ""), 
    y(1539, 2, 28, ""),
    y(1540, 2, 28, ""), 
    y(1541, 2, 28, ""),
    y(1542, 2, 28, ""), 
    y(1543, 2, 28, ""),
    y(1544, 2, 28, ""), 
    y(1545, 2, 28, ""),
    y(1546, 2, 28, ""), 
    y(1547, 2, 28, ""),
    y(1548, 2, 28, ""), 
    y(1549, 2, 28, ""),
    y(1550, 2, 28, ""), 
    y(1551, 2, 28, ""),
    y(1552, 2, 28, ""), 
    y(1553, 2, 28, ""),
    y(1554, 2, 28, ""), 
    y(1555, 2, 28, ""),
    y(1556, 2, 28, ""), 
    y(1557, 2, 28, ""),
    y(1558, 2, 28, ""), 
    y(1559, 2, 28, ""),
    y(1560, 2, 28, ""),
    y(1561, 2, 28, ""),
    y(1562, 2, 28, ""), 
    y(1563, 2, 28, ""),
    y(1564, 2, 28, ""), 
    y(1565, 2, 28, ""),
    y(1566, 2, 28, ""), 
    y(1567, 2, 28, ""),
    y(1568, 2, 28, ""), 
    y(1569, 2, 28, ""),
    y(1570, 2, 28, ""), 
    y(1571, 2, 28, ""),
    y(1572, 2, 28, ""), 
    y(1573, 2, 28, ""),
    y(1574, 2, 28, ""), 
    y(1575, 2, 28, ""),
    y(1576, 2, 28, ""), 
    y(1577, 2, 28, ""),
    y(1578, 2, 28, ""), 
    y(1579, 2, 28, ""),
    y(1580, 2, 28, ""),
    y(1581, 2, 28, ""),
    y(1582, 2, 28, ""), 
    y(1583, 2, 28, ""),
    y(1584, 2, 28, ""), 
    y(1585, 2, 28, ""),
    y(1586, 2, 28, ""), 
    y(1587, 2, 28, ""),
    y(1588, 2, 28, ""), 
    y(1589, 2, 28, ""),
    y(1590, 2, 28, ""), 
    y(1591, 2, 28, ""),
    y(1592, 2, 28, ""), 
    y(1593, 2, 28, ""),
    y(1594, 2, 28, ""), 
    y(1595, 2, 28, ""),
    y(1596, 2, 28, ""), 
    y(1597, 2, 28, ""),
    y(1598, 2, 28, ""), 
    y(1599, 2, 28, ""),
    y(1600, 2, 28, ""),
    y(1601, 2, 28, ""),
    y(1602, 2, 28, ""), 
    y(1603, 2, 28, ""),
    y(1604, 2, 28, ""), 
    y(1605, 2, 28, ""),
    y(1606, 2, 28, ""), 
    y(1607, 2, 28, ""),
    y(1608, 2, 28, ""), 
    y(1609, 2, 28, ""),
    y(1610, 2, 28, ""), 
    y(1611, 2, 28, ""),
    y(1612, 2, 28, ""), 
    y(1613, 2, 28, ""),
    y(1614, 2, 28, ""), 
    y(1615, 2, 28, ""),
    y(1616, 2, 28, ""), 
    y(1617, 2, 28, ""),
    y(1618, 2, 28, ""), 
    y(1619, 2, 28, ""),
    y(1620, 2, 28, ""), 
    y(1621, 2, 28, ""),
    y(1622, 2, 28, ""), 
    y(1623, 2, 28, ""),
    y(1624, 2, 28, ""), 
    y(1625, 2, 28, ""),
    y(1626, 2, 28, ""), 
    y(1627, 2, 28, ""),
    y(1628, 2, 28, ""), 
    y(1629, 2, 28, ""),
    y(1630, 2, 28, ""), 
    y(1631, 2, 28, ""),
    y(1632, 2, 28, ""), 
    y(1633, 2, 28, ""),
    y(1634, 2, 28, ""), 
    y(1635, 2, 28, ""),
    y(1636, 2, 28, ""), 
    y(1637, 2, 28, ""),
    y(1638, 2, 28, ""), 
    y(1639, 2, 28, ""),
    y(1640, 2, 28, ""), 
    y(1641, 2, 28, ""),
    y(1642, 2, 28, ""), 
    y(1643, 2, 28, ""),
    y(1644, 2, 28, ""), 
    y(1645, 2, 28, ""),
    y(1646, 2, 28, ""), 
    y(1647, 2, 28, ""),
    y(1648, 2, 28, ""), 
    y(1649, 2, 28, ""),
    y(1650, 2, 28, ""), 
    y(1651, 2, 28, ""),
    y(1652, 2, 28, ""), 
    y(1653, 2, 28, ""),
    y(1654, 2, 28, ""), 
    y(1655, 2, 28, ""),
    y(1656, 2, 28, ""), 
    y(1657, 2, 28, ""),
    y(1658, 2, 28, ""), 
    y(1659, 2, 28, ""),
    y(1660, 2, 28, ""),
    y(1661, 2, 28, ""),
    y(1662, 2, 28, ""), 
    y(1663, 2, 28, ""),
    y(1664, 2, 28, ""), 
    y(1665, 2, 28, ""),
    y(1666, 2, 28, ""), 
    y(1667, 2, 28, ""),
    y(1668, 2, 28, ""), 
    y(1669, 2, 28, ""),
    y(1670, 2, 28, ""), 
    y(1671, 2, 28, ""),
    y(1672, 2, 28, ""), 
    y(1673, 2, 28, ""),
    y(1674, 2, 28, ""), 
    y(1675, 2, 28, ""),
    y(1676, 2, 28, ""), 
    y(1677, 2, 28, ""),
    y(1678, 2, 28, ""), 
    y(1679, 2, 28, ""),
    y(1680, 2, 28, ""),
    y(1681, 2, 28, ""),
    y(1682, 2, 28, ""), 
    y(1683, 2, 28, ""),
    y(1684, 2, 28, ""), 
    y(1685, 2, 28, ""),
    y(1686, 2, 28, ""), 
    y(1687, 2, 28, ""),
    y(1688, 2, 28, ""), 
    y(1689, 2, 28, ""),
    y(1690, 2, 28, ""), 
    y(1691, 2, 28, ""),
    y(1692, 2, 28, ""), 
    y(1693, 2, 28, ""),
    y(1694, 2, 28, ""), 
    y(1695, 2, 28, ""),
    y(1696, 2, 28, ""), 
    y(1697, 2, 28, ""),
    y(1698, 2, 28, ""), 
    y(1699, 2, 28, ""),    
    y(1700, 2, 28, ""),
    y(1701, 2, 28, ""),
    y(1702, 2, 28, ""), 
    y(1703, 2, 28, ""),
    y(1704, 2, 28, ""), 
    y(1705, 2, 28, ""),
    y(1706, 2, 28, ""), 
    y(1707, 2, 28, ""),
    y(1708, 2, 28, ""), 
    y(1709, 2, 28, ""),
    y(1710, 2, 28, ""), 
    y(1711, 2, 28, ""),
    y(1712, 2, 28, ""), 
    y(1713, 2, 28, ""),
    y(1714, 2, 28, ""), 
    y(1715, 2, 28, ""),
    y(1716, 2, 28, ""), 
    y(1717, 2, 28, ""),
    y(1718, 2, 28, ""), 
    y(1719, 2, 28, ""),
    y(1720, 2, 28, ""), 
    y(1721, 2, 28, ""),
    y(1722, 2, 28, ""), 
    y(1723, 2, 28, ""),
    y(1724, 2, 28, ""), 
    y(1725, 2, 28, ""),
    y(1726, 2, 28, ""), 
    y(1727, 2, 28, ""),
    y(1728, 2, 28, ""), 
    y(1729, 2, 28, ""),
    y(1730, 2, 28, ""), 
    y(1731, 2, 28, ""),
    y(1732, 2, 28, ""), 
    y(1733, 2, 28, ""),
    y(1734, 2, 28, ""), 
    y(1735, 2, 28, ""),
    y(1736, 2, 28, ""), 
    y(1737, 2, 28, ""),
    y(1738, 2, 28, ""), 
    y(1739, 2, 28, ""),
    y(1740, 2, 28, ""), 
    y(1741, 2, 28, ""),
    y(1742, 2, 28, ""), 
    y(1743, 2, 28, ""),
    y(1744, 2, 28, ""), 
    y(1745, 2, 28, ""),
    y(1746, 2, 28, ""), 
    y(1747, 2, 28, ""),
    y(1748, 2, 28, ""), 
    y(1749, 2, 28, ""),
    y(1750, 2, 28, ""), 
    y(1751, 2, 28, ""),
    y(1752, 2, 28, ""), 
    y(1753, 2, 28, ""),
    y(1754, 2, 28, ""), 
    y(1755, 2, 28, ""),
    y(1756, 2, 28, ""), 
    y(1757, 2, 28, ""),
    y(1758, 2, 28, ""), 
    y(1759, 2, 28, ""),
    y(1760, 2, 28, ""),
    y(1761, 2, 28, ""),
    y(1762, 2, 28, ""), 
    y(1763, 2, 28, ""),
    y(1764, 2, 28, ""), 
    y(1765, 2, 28, ""),
    y(1766, 2, 28, ""), 
    y(1767, 2, 28, ""),
    y(1768, 2, 28, ""), 
    y(1769, 2, 28, ""),
    y(1770, 2, 28, ""), 
    y(1771, 2, 28, ""),
    y(1772, 2, 28, ""), 
    y(1773, 2, 28, ""),
    y(1774, 2, 28, ""), 
    y(1775, 2, 28, ""),
    y(1776, 2, 28, ""), 
    y(1777, 2, 28, ""),
    y(1778, 2, 28, ""), 
    y(1779, 2, 28, ""),
    y(1780, 2, 28, ""),
    y(1781, 2, 28, ""),
    y(1782, 2, 28, ""), 
    y(1783, 2, 28, ""),
    y(1784, 2, 28, ""), 
    y(1785, 2, 28, ""),
    y(1786, 2, 28, ""), 
    y(1787, 2, 28, ""),
    y(1788, 2, 28, ""), 
    y(1789, 2, 28, ""),
    y(1790, 2, 28, ""), 
    y(1791, 2, 28, ""),
    y(1792, 2, 28, ""), 
    y(1793, 2, 28, ""),
    y(1794, 2, 28, ""), 
    y(1795, 2, 28, ""),
    y(1796, 2, 28, ""), 
    y(1797, 2, 28, ""),
    y(1798, 2, 28, ""), 
    y(1799, 2, 28, ""),
    y(1800, 2, 28, ""),
    y(1801, 2, 28, ""),
    y(1802, 2, 28, ""), 
    y(1803, 2, 28, ""),
    y(1804, 2, 28, ""), 
    y(1805, 2, 28, ""),
    y(1806, 2, 28, ""), 
    y(1807, 2, 28, ""),
    y(1808, 2, 28, ""), 
    y(1809, 2, 28, ""),
    y(1810, 2, 28, ""), 
    y(1811, 2, 28, ""),
    y(1812, 2, 28, ""), 
    y(1813, 2, 28, ""),
    y(1814, 2, 28, ""), 
    y(1815, 2, 28, ""),
    y(1816, 2, 28, ""), 
    y(1817, 2, 28, ""),
    y(1818, 2, 28, ""), 
    y(1819, 2, 28, ""),
    y(1820, 2, 28, ""), 
    y(1821, 2, 28, ""),
    y(1822, 2, 28, ""), 
    y(1823, 2, 28, ""),
    y(1824, 2, 28, ""), 
    y(1825, 2, 28, ""),
    y(1826, 2, 28, ""), 
    y(1827, 2, 28, ""),
    y(1828, 2, 28, ""), 
    y(1829, 2, 28, ""),
    y(1830, 2, 28, ""), 
    y(1831, 2, 28, ""),
    y(1832, 2, 28, ""), 
    y(1833, 2, 28, ""),
    y(1834, 2, 28, ""), 
    y(1835, 2, 28, ""),
    y(1836, 2, 28, ""), 
    y(1837, 2, 28, ""),
    y(1838, 2, 28, ""), 
    y(1839, 2, 28, ""),
    y(1840, 2, 28, ""), 
    y(1841, 2, 28, ""),
    y(1842, 2, 28, ""), 
    y(1843, 2, 28, ""),
    y(1844, 2, 28, ""), 
    y(1845, 2, 28, ""),
    y(1846, 2, 28, ""), 
    y(1847, 2, 28, ""),
    y(1848, 2, 28, ""), 
    y(1849, 2, 28, ""),
    y(1850, 2, 28, ""), 
    y(1851, 2, 28, ""),
    y(1852, 2, 28, ""), 
    y(1853, 2, 28, ""),
    y(1854, 2, 28, ""), 
    y(1855, 2, 28, ""),
    y(1856, 2, 28, ""), 
    y(1857, 2, 28, ""),
    y(1858, 2, 28, ""), 
    y(1859, 2, 28, ""),
    y(1860, 2, 28, ""),
    y(1861, 2, 28, ""),
    y(1862, 2, 28, ""), 
    y(1863, 2, 28, ""),
    y(1864, 2, 28, ""), 
    y(1865, 2, 28, ""),
    y(1866, 2, 28, ""), 
    y(1867, 2, 28, ""),
    y(1868, 2, 28, ""), 
    y(1869, 2, 28, ""),
    y(1870, 2, 28, ""), 
    y(1871, 2, 28, ""),
    y(1872, 2, 28, ""), 
    y(1873, 2, 28, ""),
    y(1874, 2, 28, ""), 
    y(1875, 2, 28, ""),
    y(1876, 2, 28, ""), 
    y(1877, 2, 28, ""),
    y(1878, 2, 28, ""), 
    y(1879, 2, 28, ""),
    y(1880, 2, 28, ""),
    y(1881, 2, 28, ""),
    y(1882, 2, 28, ""), 
    y(1883, 2, 28, ""),
    y(1884, 2, 28, ""), 
    y(1885, 2, 28, ""),
    y(1886, 2, 28, ""), 
    y(1887, 2, 28, ""),
    y(1888, 2, 28, ""), 
    y(1889, 2, 28, ""),
    y(1890, 2, 28, ""), 
    y(1891, 2, 28, ""),
    y(1892, 2, 28, ""), 
    y(1893, 2, 28, ""),
    y(1894, 2, 28, ""), 
    y(1895, 2, 28, ""),
    y(1896, 2, 28, ""), 
    y(1897, 2, 28, ""),
    y(1898, 2, 28, ""), 
    y(1899, 2, 28, ""),
    y(1900, 2, 28, ""),
    y(1901, 2, 28, ""),
    y(1902, 2, 28, ""), 
    y(1903, 2, 28, ""),
    y(1904, 2, 28, ""), 
    y(1905, 2, 28, ""),
    y(1906, 2, 28, ""), 
    y(1907, 2, 28, ""),
    y(1908, 2, 28, ""), 
    y(1909, 2, 28, ""),
    y(1910, 2, 28, ""), 
    y(1911, 2, 28, "")
  )
  private val ShuYears = Array(
    ce223, ce224, ce225, ce226, ce227, ce228, ce229, ce230, ce231, ce232, ce233,
    ce234, ce235, ce236,
    y(237, 2, 13, "己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子"), // 三月，魏改元“景初”，建丑，蜀仍用四分历
    y(238, 2, 2,  "癸巳 癸亥 壬辰 壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 閏 己丑 戊午 戊子"), 
    y(239, 2, 21, "丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 甲寅 癸未 癸丑 壬午"),
    y(240, 2, 11, "壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丁丑"), 
    y(241, 1, 30, "丙午 丙子 乙巳 乙亥 甲辰 甲戌 閏 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子"),
    y(242, 2, 18, "庚午 己亥 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未"),
    y(243, 2, 7,  "甲子 甲午 癸亥 癸巳 壬戌 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑"),
    y(244, 1, 28, "己未 戊子 戊午 閏 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 甲申 癸丑"), 
    y(245, 2, 15, "癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 丁未"),
    y(246, 2, 4,  "丁丑 丙午 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯 壬申 閏 壬寅 辛未"), 
    y(247, 2, 23, "辛丑 庚午 庚子 己巳 己亥 己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅"),
    y(248, 2, 12, "乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 辛卯 庚申"),
    y(249, 2, 1,  "庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 閏 丙戌 乙卯 乙酉 甲寅 甲申"),
    y(250, 2, 20, "甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅"),
    y(251, 2, 9,  "戊申 丁丑 丁未 丙子 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉"),
    y(252, 1, 29, "壬寅 壬申 辛丑 辛未 庚子 閏 庚午 己亥 己巳 戊戌 戊辰 戊戌 丁卯 丁酉"), 
    y(253, 2, 16, "丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯"),
    y(254, 2, 6,  "辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉"), 
    y(255, 1, 26, "乙卯 閏 甲申 甲寅 癸未 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉"),
    y(256, 2, 14, "己卯 戊申 戊寅 丁未 丁丑 丙午 丙子 丙午 乙亥 乙巳 甲戌 甲辰"), 
    y(257, 2, 2,  "癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 閏 己亥 戊辰 戊戌 戊辰"),
    y(258, 2, 21, "丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌"), 
    y(259, 2, 10, "辛卯 辛酉 庚寅 庚申 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰"),
    y(260, 1, 31, "丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 閏 癸丑 壬午 壬子 辛巳 辛亥 庚辰"),
    y(261, 2, 18, "庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 乙亥"),
    y(262, 2, 7,  "甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳"), 
    y(263, 1, 27, "戊戌 戊辰 戊戌 丁卯 閏 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳")
  )
  private val WuYears = Array(
    ce222,
    y(223, 2, 18, "庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉 乙卯"),
    y(224, 2, 7,  "甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉"),
    y(225, 1, 26, "戊寅 戊申 戊寅 丁未 閏 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉"),
    y(226, 2, 14, "壬寅 壬申 辛丑 辛未 庚子 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯"), 
    y(227, 2, 4,  "丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 壬戌 閏 辛卯"),    
    y(228, 2, 23, "辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉"),     
    y(229, 2, 11, "乙卯 甲申 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰"),
    y(230, 1, 31, "己酉 己卯 戊申 戊寅 丁未 丁丑 丁未 丙子 閏 丙午 乙亥 乙巳 甲戌 甲辰"),
    y(231, 2, 19, "癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 己巳 戊戌"),
    y(232, 2, 9,  "戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰"),
    y(233, 1, 28, "壬戌 辛卯 辛酉 辛卯 庚申 閏 庚寅 己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 "),
    y(234, 2, 16, "丙戌 乙卯 乙酉 甲寅 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥"),
    y(235, 2, 5,  "庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 丙子 乙巳"),
    y(236, 1, 26, "乙亥 甲辰 閏 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳"),
    y(237, 2, 12, "戊戌 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午 癸亥"), 
    y(238, 2, 2,  "癸巳 壬戌 壬辰 辛酉 辛卯 辛酉 庚寅 庚申 己丑 己未 閏 戊子 戊午 丁亥"), 
    y(239, 2, 21, "丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 癸未 壬子 壬午"),    
    ce240,
    y(241, 1, 29, "乙巳 乙亥 乙巳 甲戌 甲辰 癸酉 閏 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子"),
    y(242, 2, 17, "己巳 己亥 戊辰 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 乙丑 甲午"),
    y(243, 2, 7,  "甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 庚寅 己未 己丑"),
    y(244, 1, 27, "戊午 戊子 丁巳 閏 丁亥 丙辰 丙戌 乙卯 乙酉 甲寅 甲申 癸丑 癸未 癸丑"),
    y(245, 2, 14, "壬午 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 戊申 丁丑 丁未"),
    y(246, 2, 3,  "丙子 丙午 乙亥 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 閏 辛未"),
    ce247, ce248,
    y(249, 1, 31, "己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 閏 乙酉 乙卯 甲申 甲寅 癸未"),
    y(250, 2, 19, "癸丑 壬午 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅"), 
    y(251, 2, 8,  "丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 甲辰 癸酉 癸卯 壬申"),
    y(252, 1, 29, "壬寅 辛未 辛丑 庚午 閏 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申"), 
    y(253, 2, 16, "丙寅 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅"),
    y(254, 2, 5,  "庚申 己丑 己未 戊子 戊午 戊子 丁巳 丁亥 丙辰 丙戌 乙卯 乙酉"), 
    y(255, 1, 25, "甲寅 閏 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 辛亥 庚辰 庚戌 己卯 己酉"),
    // This is same as Wu Years. The reason we duplicate is that the
    // 1st day of new year is different, so we need two copies to keep the month length
    // of the last month of CE 256.
    y(256, 2, 13, "戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉 癸卯"),
    y(257, 2, 2,  "癸酉 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥 己巳 戊戌 閏 戊辰 丁酉 丁卯"),
    y(258, 2, 20, "丙申 丙寅 乙未 乙丑 乙未 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉"), 
    y(259, 2, 10, "辛卯 庚申 庚寅 己未 己丑 戊午 戊子 戊午 丁亥 丁巳 丙戌 丙辰"),
    y(260, 1, 30, "乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 閏 壬子 辛巳 辛亥 庚辰 庚戌 庚辰"),
    ce261,
    y(262, 2, 6,  "癸卯 癸酉 壬寅 壬申 壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰"), 
    y(263, 1, 27, "戊戌 丁卯 丁酉 閏 丙寅 丙申 乙丑 乙未 乙丑 甲午 甲子 癸巳 癸亥 壬辰"),
    y(264, 2, 15, "壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丁亥"),
    ce265,
    y(266, 2, 22, "庚辰 己酉 己卯 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳"), 
    y(267, 2, 11, "甲戌 甲辰 癸酉 癸卯 壬申 壬寅 壬申 辛丑 辛未 庚子 庚午 己亥"),
    y(268, 2, 1,  "己巳 戊戌 戊辰 丁酉 丁卯 丙申 丙寅 乙未 閏 乙丑 甲午 甲子 甲午 癸亥"),
    ce269,
    y(270, 2, 8,  "丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子"), 
    y(271, 1, 28, "辛巳 辛亥 庚辰 庚戌 己卯 閏 己酉 己卯 戊申 戊寅 丁未 丁丑 丙午 丙子"),
    y(272, 2, 16, "乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅 壬申 辛丑 辛未 辛丑 庚午"),
    ce273,
    y(274, 1, 25, "甲午 閏 癸亥 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子"),
    ce275,
    y(276, 2, 2,  "壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉 己卯 戊申 戊寅 閏 戊申 丁丑 丁未"),
    ce277, ce278, ce279, ce280
  )

  private val BeiWeiYears = Array(
    ce440, ce441, ce442, ce443, ce444,
    y(445, 1, 24, "辛卯 閏 辛酉 庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉"),
    ce446,
    y(447, 2, 1,  "己酉 己卯 戊申 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 閏 乙巳 乙亥 甲辰"),
    y(448, 2, 20, "癸酉 癸卯 壬申 壬寅 辛未 辛丑 庚午 庚子 庚午 己亥 己巳 戊戌"), 
    ce449,
    y(450, 1, 29, "壬戌 壬辰 辛酉 辛卯 庚申 庚寅 己未 閏 己丑 戊午 戊子 丁巳 丁亥 丙辰"),
    ce451, ce452, ce453, ce454,
    y(455, 2, 3,  "癸巳 壬戌 壬辰 辛酉 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子 戊午"),
    y(456, 1, 23, "丁亥 丁巳 閏 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 甲申 癸丑 癸未 壬子 壬午"),
    ce457, ce458, ce459, ce460, ce461, ce462, ce463,
    y(464, 1, 25, "辛未 庚子 庚午 己亥 閏 己巳 戊戌 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑"),
    ce465, ce466, ce467, ce468,
    y(469, 1, 29, "壬寅 辛未 辛丑 庚午 庚子 己巳 己亥 戊辰 戊戌 閏 丁卯 丁酉 丁卯 丙申"),
    ce470, ce471, 
    y(472, 1, 26, "甲寅 甲申 癸丑 癸未 壬子 壬午 閏 壬子 辛巳 辛亥 庚辰 庚戌 己卯 己酉"),
    ce473, ce474, ce475, ce476, 
    y(477, 1, 30, "乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 辛巳 閏 庚戌 庚辰"),
    ce478, ce479, ce480, ce481, ce482,
    y(483, 1, 24, "庚戌 庚辰 庚戌 己卯 閏 己酉 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳"),
    ce484, ce485, ce486, ce487,
    y(488, 1, 29, "辛巳 辛亥 庚辰 庚戌 庚辰 己酉 己卯 戊申 戊寅 閏 丁未 丁丑 丙午 丙子"),
    ce489, ce490, ce491, ce492, ce493, ce494, ce495,
    y(496, 1, 31, "乙丑 甲午 甲子 甲午 癸亥 癸巳 壬戌 壬辰 辛酉 辛卯 庚申 閏 庚寅 己未"),
    ce497, ce498, ce499, ce500, ce501, ce502,    
    y(503, 2, 28, ""),
    y(504, 2, 28, ""), 
    y(505, 2, 28, ""),
    y(506, 2, 28, ""), 
    y(507, 2, 28, ""),
    y(508, 2, 28, ""), 
    y(509, 2, 28, ""),
    y(510, 2, 28, ""), 
    y(511, 2, 28, ""),
    y(512, 2, 28, ""), 
    y(513, 2, 28, ""),
    y(514, 2, 28, ""), 
    y(515, 2, 28, ""),
    y(516, 2, 28, ""), 
    y(517, 2, 28, ""),
    y(518, 2, 28, ""), 
    y(519, 2, 28, ""),
    y(520, 2, 28, ""), 
    y(521, 2, 28, ""),
    y(522, 2, 28, ""), 
    y(523, 2, 28, ""),
    y(524, 2, 28, ""), 
    y(525, 2, 28, ""),
    y(526, 2, 28, ""), 
    y(527, 2, 28, ""),
    y(528, 2, 28, ""), 
    y(529, 2, 28, ""),
    y(530, 2, 28, ""), 
    y(531, 2, 28, ""),
    y(532, 2, 28, ""), 
    y(533, 2, 28, ""),
    y(534, 2, 28, ""), 
    y(535, 2, 28, ""),
    y(536, 2, 28, ""), 
    y(537, 2, 28, ""),
    y(538, 2, 28, ""), 
    y(539, 2, 28, ""),
    y(540, 2, 28, ""), 
    y(541, 2, 28, ""),
    y(542, 2, 28, ""), 
    y(543, 2, 28, ""),
    y(544, 2, 28, ""), 
    y(545, 2, 28, ""),
    y(546, 2, 28, ""), 
    y(547, 2, 28, ""),
    y(548, 2, 28, ""), 
    y(549, 2, 28, ""),
    y(550, 2, 28, ""), 
    y(551, 2, 28, ""),
    y(552, 2, 28, ""), 
    y(553, 2, 28, ""),
    y(554, 2, 28, ""), 
    y(555, 2, 28, ""),
    y(556, 2, 28, ""), 
    y(557, 2, 28, ""),
    y(558, 2, 28, ""), 
    y(559, 2, 28, ""),
    y(560, 2, 28, ""),
    y(561, 2, 28, ""),
    y(562, 2, 28, ""), 
    y(563, 2, 28, ""),
    y(564, 2, 28, ""), 
    y(565, 2, 28, ""),
    y(566, 2, 28, ""), 
    y(567, 2, 28, ""),
    y(568, 2, 28, ""), 
    y(569, 2, 28, ""),
    y(570, 2, 28, ""), 
    y(571, 2, 28, ""),
    y(572, 2, 28, ""), 
    y(573, 2, 28, ""),
    y(574, 2, 28, ""), 
    y(575, 2, 28, ""),
    y(576, 2, 28, ""), 
    y(577, 2, 28, ""),
    y(578, 2, 28, ""), 
    y(579, 2, 28, ""),
    y(580, 2, 28, "")  
)

  // The tuple has five elements:
  // - 1st: era name
  // - 2nd and 3rd indicates the duration of the period, as start & end.
  //   If one era corresponds to multiple intervals, each interval should
  //   be entered separately.
  //   If start and end are non-empty, the concatenation of era name and start/end
  //   should be a valid date of ChineseCalendar (i.e. the concatenation
  //   with possibly addition of 元年 for start_* can be passed to
  //   toDate() without any error/exception).
  //   For *start*:
  //     Empty string indicates that the era starts at the new year.
  //     The default date for *start* is 初一,
  //   For *end*:
  //     Empty string indicates that it is calculated based on the
  //       start of next era.
  //   Default date for *end* is the end of the month.
  // - 4th: the previous era.
  //   Empty string is used to indicate the previous entry in the array.
  //   Needed for navigation through months.
  // - 5th: the next era.
  //   Empty string is used to indicate the next entry in the array.
  //   Needed for navigation through months.  
  // - 6th: another tuple, with the 1st element indicating the corresponding table,
  //   while the 2nd element is the start year in Julian/Gregorian Calendar.
  //
  // There is no requirement on the order of the start date, since it
  // is convenient to input data for a dynasty consecuritively when
  // there are several competing dynasties.
  private val eraArray = Array(
    ("秦孝文王", "", "", "", "", (BCEYears, -250)),
    ("秦莊襄王", "", "", "", "", (BCEYears, -249)),    
    ("秦王政", "", "", "", "", (BCEYears, -246)),
    ("秦始皇", "二十六年", "", "", "", (BCEYears, -246)),
    ("秦二世", "", "", "", "", (BCEYears, -209)),
    ("漢高祖", "", "", "", "", (BCEYears, -206)),
    ("漢惠帝", "", "", "", "", (BCEYears, -194)),
    ("漢高后", "", "", "", "", (BCEYears, -187)),      
    ("漢文帝", "", "", "", "", (BCEYears, -179)),
    ("漢文帝後", "", "", "", "", (BCEYears, -163)),       
    ("漢景帝", "", "", "", "", (BCEYears, -156)),
    ("漢景帝中", "", "", "", "", (BCEYears, -149)),
    ("漢景帝後", "", "", "", "", (BCEYears, -143)),            
    ("漢武帝建元", "", "", "", "", (BCEYears, -140)),
    ("漢武帝元光", "", "", "", "", (BCEYears, -134)),
    ("漢武帝元朔", "", "", "", "", (BCEYears, -128)),
    ("漢武帝元狩", "", "", "", "", (BCEYears, -122)),
    ("漢武帝元鼎", "", "", "", "", (BCEYears, -116)),
    ("漢武帝元封", "", "", "", "", (BCEYears, -110)),
    ("漢武帝太初", "", "", "", "", (BCEYears, -104)),
    ("漢武帝天漢", "", "", "", "", (BCEYears, -99)),
    ("漢武帝太始", "", "", "", "", (BCEYears, -95)),
    ("漢武帝征和", "", "", "", "", (BCEYears, -91)),
    ("漢武帝後元", "", "", "", "", (BCEYears, -87)),        
    ("漢昭帝始元", "", "", "", "", (BCEYears, -85)),
    ("漢昭帝元鳳", "八月", "", "", "", (BCEYears, -79)),
    ("漢昭帝元平", "", "", "", "", (BCEYears, -73)),        
    ("漢宣帝本始", "", "", "", "", (BCEYears, -72)),
    ("漢宣帝地節", "", "", "", "", (BCEYears, -68)),
    ("漢宣帝元康", "", "", "", "", (BCEYears, -64)),
    ("漢宣帝神爵", "三月", "", "", "", (BCEYears, -60)),
    ("漢宣帝五鳳", "", "", "", "", (BCEYears, -56)),
    ("漢宣帝甘露", "", "", "", "", (BCEYears, -52)),        
    ("漢宣帝黃龍", "", "", "", "", (BCEYears, -48)),    
    ("漢元帝初元", "", "", "", "", (BCEYears, -47)),
    ("漢元帝永光", "", "", "", "", (BCEYears, -42)),
    ("漢元帝建昭", "", "", "", "", (BCEYears, -37)),        
    ("漢元帝竟寧", "", "", "", "", (BCEYears, -32)),    
    ("漢成帝建始", "", "", "", "", (BCEYears, -31)),
    ("漢成帝河平", "三月", "", "", "", (BCEYears, -27)),
    ("漢成帝陽朔", "", "", "", "", (BCEYears, -23)),
    ("漢成帝鴻嘉", "", "", "", "", (BCEYears, -19)),
    ("漢成帝永始", "", "", "", "", (BCEYears, -15)),
    ("漢成帝元延", "", "", "", "", (BCEYears, -11)),
    ("漢成帝綏和", "", "", "", "", (BCEYears, -7)),
    ("漢哀帝建平", "", "", "", "", (BCEYears, -5)),
    ("漢哀帝太初元將", "六月", "", "", "", (BCEYears, -4)),
    ("漢哀帝建平", "二年八月", "", "", "", (BCEYears, -5)),    
    ("漢哀帝元壽", "", "", "", "", (BCEYears, -1)),        
    ("漢平帝元始", "", "", "", "", (CEYears, 1)),
    ("漢孺子嬰居攝", "", "", "", "", (CEYears, 6)),
    ("漢孺子嬰初始", "十一月", "", "", "", (CEYears, 8)),
    ("新王莽始建國", "", "", "", "", (CEYears, 9)),
    ("新王莽天鳳", "", "", "", "", (CEYears, 14)),
    ("新王莽地皇", "", "四年九月", "", "", (CEYears, 20)),
    ("劉玄更始", "二月", "三年九月", "", "", (CEYears, 23)),
    ("漢光武帝建武", "六月", "", "", "", (CEYears, 25)),
    ("漢光武帝建武中元", "四月", "", "", "", (CEYears, 56)),
    ("漢明帝永平", "", "", "", "", (CEYears, 58)),
    ("漢章帝建初", "", "", "", "", (CEYears, 76)),
    ("漢章帝元和", "八月", "", "", "", (CEYears, 84)),
    ("漢章帝章和", "七月", "", "", "", (CEYears, 87)),
    ("漢和帝永元", "", "", "", "", (CEYears, 89)),
    ("漢和帝元興", "四月", "", "", "", (CEYears, 105)),
    ("漢殤帝延平", "", "", "", "", (CEYears, 106)),
    ("漢安帝永初", "", "", "", "", (CEYears, 107)),
    ("漢安帝元初", "", "", "", "", (CEYears, 114)),
    ("漢安帝永寧", "四月", "", "", "", (CEYears, 120)),
    ("漢安帝建光", "七月", "", "", "", (CEYears, 121)),
    ("漢安帝延光", "三月", "", "", "", (CEYears, 122)),
    ("漢順帝永建", "", "", "", "", (CEYears, 126)),
    ("漢順帝陽嘉", "三月", "", "", "", (CEYears, 132)),
    ("漢順帝永和", "", "", "", "", (CEYears, 136)),
    ("漢順帝漢安", "", "", "", "", (CEYears, 142)),
    ("漢順帝建康", "四月", "", "", "", (CEYears, 144)),
    ("漢沖帝永憙", "", "", "", "", (CEYears, 145)),
    ("漢質帝本初", "", "", "", "", (CEYears, 146)),
    ("漢桓帝建和", "", "", "", "", (CEYears, 147)),
    ("漢桓帝和平", "", "", "", "", (CEYears, 150)),
    ("漢桓帝元嘉", "", "", "", "", (CEYears, 151)),
    ("漢桓帝永興", "五月", "", "", "", (CEYears, 153)),
    ("漢桓帝永壽", "", "", "", "", (CEYears, 155)),
    ("漢桓帝延熹", "六月", "", "", "", (CEYears, 158)),
    ("漢桓帝永康", "六月", "", "", "", (CEYears, 167)),
    ("漢靈帝建寧", "", "", "", "", (CEYears, 168)),
    ("漢靈帝熹平", "五月", "", "", "", (CEYears, 172)),
    ("漢靈帝光和", "三月", "", "", "", (CEYears, 178)),
    ("漢靈帝中平", "十二月", "", "", "", (CEYears, 184)),
    ("漢少帝光熹", "四月", "", "", "", (CEYears, 189)),
    ("漢少帝昭寧", "八月", "", "", "", (CEYears, 189)),
    ("漢獻帝永漢", "九月", "", "", "", (CEYears, 189)),
    ("漢獻帝中平", "六年十二月", "", "", "", (CEYears, 184)),      
    ("漢獻帝初平", "", "", "", "", (CEYears, 190)),
    ("漢獻帝興平", "", "", "", "", (CEYears, 194)),
    ("漢獻帝建安", "", "", "", "", (CEYears, 196)),
    ("漢獻帝延康", "三月", "", "", "", (CEYears, 220)),
    ("魏文帝黃初", "十月", "", "", "", (CEYears, 220)),
    ("魏明帝太和", "", "", "", "", (CEYears, 227)),
    ("魏明帝青龍", "二月", "", "", "", (CEYears, 233)),
    ("魏明帝景初", "四月", "", "", "", (CEYears, 237)),
    ("魏齊王芳正始", "", "", "", "", (CEYears, 240)),
    ("魏齊王芳嘉平", "四月", "", "", "", (CEYears, 249)),
    ("魏高貴鄉公正元", "十月", "", "", "", (CEYears, 254)),
    ("魏高貴鄉公甘露", "六月", "", "", "", (CEYears, 256)),
    ("魏陳留王景元", "六月", "", "", "", (CEYears, 260)),
    ("魏陳留王咸熙", "五月", "", "", "晉武帝泰始", (CEYears, 264)),    
    ("蜀昭烈帝章武", "四月", "", "魏文帝黃初", "", (CEYears, 221)),
    ("蜀後主建興", "五月", "", "", "", (ShuYears, 223)),
    ("蜀後主延熙", "", "", "", "", (ShuYears, 238)),
    ("蜀後主景耀", "", "", "", "", (ShuYears, 258)),
    ("蜀後主炎興", "八月", "十一月", "", "魏陳留王景元", (ShuYears, 263)),                  
    ("吳大帝黃武", "十月", "", "魏文帝黃初", "", (WuYears, 222)),
    ("吳大帝黃龍", "四月", "", "", "", (WuYears, 229)),
    ("吳大帝嘉禾", "", "", "", "", (WuYears, 232)),        
    ("吳大帝赤烏", "八月", "", "", "", (WuYears, 238)),            
    ("吳大帝太元", "五月", "", "", "", (WuYears, 251)),
    ("吳大帝神鳳", "二月", "", "", "", (WuYears, 252)),
    ("吳會稽王建興", "四月", "", "", "", (WuYears, 252)),            
    ("吳會稽王五鳳", "", "", "", "", (WuYears, 254)),            
    ("吳會稽王太平", "十月", "", "", "", (WuYears, 256)),                
    ("吳景帝永安", "十月", "", "", "", (WuYears, 258)),    
    ("吳末帝元興", "七月", "", "", "", (WuYears, 264)),
    ("吳末帝甘露", "四月", "", "", "", (WuYears, 265)),
    ("吳末帝寶鼎", "八月", "", "", "", (WuYears, 266)),
    ("吳末帝建衡", "十月", "", "", "", (WuYears, 269)),
    ("吳末帝鳳凰", "", "", "", "", (WuYears, 272)),
    ("吳末帝天冊", "", "", "", "", (WuYears, 275)),        
    ("吳末帝天璽", "七月", "", "", "", (WuYears, 276)),
    ("吳末帝天紀", "", "三月", "", "晉武帝太康", (WuYears, 277)),      
    ("晉武帝泰始", "十二月", "", "魏陳留王咸熙", "", (CEYears, 265)),
    ("晉武帝咸寧", "", "", "", "", (CEYears, 275)),
    ("晉武帝太康", "四月", "", "", "", (CEYears, 280)),
    ("晉武帝太熙", "", "", "", "", (CEYears, 290)),
    ("晉惠帝永熙", "四月", "", "", "", (CEYears, 290)),    
    ("晉惠帝永平", "", "", "", "", (CEYears, 291)),
    ("晉惠帝元康", "三月", "", "", "", (CEYears, 291)),    
    ("晉惠帝永康", "", "", "", "", (CEYears, 300)),
    ("晉惠帝永寧", "四月", "", "", "", (CEYears, 301)),
    ("晉惠帝太安", "十二月", "", "", "", (CEYears, 302)),
    ("晉惠帝永安", "", "", "", "", (CEYears, 304)),
    ("晉惠帝建武", "七月", "", "", "", (CEYears, 304)),
    ("晉惠帝永興", "十二月", "", "", "", (CEYears, 304)),    
    ("晉惠帝光熙", "六月", "", "", "", (CEYears, 306)),
    ("晉懷帝永嘉", "", "", "", "", (CEYears, 307)),
    ("晉愍帝建興", "四月", "", "", "", (CEYears, 313)),
    ("晉元帝建武", "三月", "", "", "", (CEYears, 317)),
    ("晉元帝大興", "三月", "", "", "", (CEYears, 318)),
    ("晉元帝永昌", "", "", "", "", (CEYears, 322)),
    ("晉明帝太寧", "三月", "", "", "", (CEYears, 323)),
    ("晉成帝咸和", "二月", "", "", "", (CEYears, 326)),
    ("晉成帝咸康", "", "", "", "", (CEYears, 335)),
    ("晉康帝建元", "", "", "", "", (CEYears, 343)),
    ("晉穆帝永和", "", "", "", "", (CEYears, 345)),
    ("晉穆帝昇平", "", "", "", "", (CEYears, 357)),
    ("晉哀帝隆和", "", "", "", "", (CEYears, 362)),
    ("晉哀帝興寧", "二月", "", "", "", (CEYears, 363)),
    ("晉廢帝太和", "", "", "", "", (CEYears, 366)),
    ("晉簡文帝咸安", "十一月", "", "", "", (CEYears, 371)),
    ("晉孝武帝寧康", "", "", "", "", (CEYears, 373)),
    ("晉孝武帝太元", "", "", "", "", (CEYears, 376)),
    ("晉安帝隆安", "", "", "", "", (CEYears, 397)),
    ("晉安帝元興", "", "", "", "", (CEYears, 402)),
    // Note used in 《資治通鑑》
    // ("晉安帝大亨", "", "", "", "", (CEYears, 402)),
    ("晉安帝義熙", "", "", "", "", (CEYears, 405)),
    ("晉恭帝元熙", "", "", "", "", (CEYears, 419)),
    ("宋武帝永初", "六月", "", "", "", (CEYears, 420)),
    ("宋少帝景平", "", "", "", "", (CEYears, 423)),
    ("宋文帝元嘉", "八月", "", "", "", (CEYears, 424)),
    ("宋孝武帝孝建", "", "", "", "", (CEYears, 454)),
    ("宋孝武帝大明", "", "", "", "", (CEYears, 457)),
    ("宋前廢帝永光", "", "", "", "", (CEYears, 465)),
    ("宋前廢帝景和", "八月", "", "", "", (CEYears, 465)),
    ("宋明帝泰始", "十二月", "", "", "", (CEYears, 465)),
    ("宋明帝泰豫", "", "", "", "", (CEYears, 472)),
    ("宋後廢帝元徽", "", "", "", "", (CEYears, 473)),
    ("宋順帝昇明", "七月", "", "", "", (CEYears, 477)),
    ("齊高帝建元", "四月", "", "", "", (CEYears, 479)),
    ("齊武帝永明", "", "", "", "", (CEYears, 483)),    
    ("齊鬱陵王隆昌", "", "", "", "", (CEYears, 494)),
    ("齊海陵王延興", "七月", "", "", "", (CEYears, 494)),
    ("齊明帝建武", "十月", "", "", "", (CEYears, 494)),
    ("齊明帝永泰", "四月", "", "", "", (CEYears, 498)),
    ("齊東昏侯永元", "", "", "", "", (CEYears, 499)),
    // TODO: remove "end"    
    ("齊和帝中興", "三月", "十二月", "", "", (CEYears, 501)),    
    ("北魏道武帝登國", "", "", "晉孝武帝太元", "", (CEYears, 386)),
    ("北魏道武帝皇始", "七月", "", "", "", (CEYears, 396)),
    ("北魏道武帝天興", "十二月", "", "", "", (CEYears, 398)),
    ("北魏道武帝天賜", "十月", "", "", "", (CEYears, 404)),
    ("北魏明元帝永興", "閏十月", "", "", "", (CEYears, 409)),
    ("北魏明元帝神瑞", "", "", "", "", (CEYears, 414)),
    ("北魏明元帝泰常", "四月", "", "", "", (CEYears, 416)),
    ("北魏太武帝始光", "", "", "", "", (CEYears, 424)),
    ("北魏太武帝神䴥", "二月", "", "", "", (CEYears, 428)),
    ("北魏太武帝延和", "", "", "", "", (CEYears, 432)),
    ("北魏太武帝太延", "", "五年", "", "", (CEYears, 435)),
    ("北魏太武帝太平真君", "六月", "", "", "", (BeiWeiYears, 440)),
    ("北魏太武帝正平", "六月", "", "", "", (BeiWeiYears, 451)),
    ("北魏南安王承平", "三月", "", "", "", (BeiWeiYears, 452)),
    ("北魏文成帝興安", "十月", "", "", "", (BeiWeiYears, 452)),
    ("北魏文成帝興光", "七月", "", "", "", (BeiWeiYears, 454)),
    ("北魏文成帝太安", "六月", "", "", "", (BeiWeiYears, 455)),
    ("北魏文成帝和平", "", "", "", "", (BeiWeiYears, 460)), 
    ("北魏獻文帝天安", "", "", "", "", (BeiWeiYears, 466)),
    ("北魏獻文帝皇興", "八月", "", "", "", (BeiWeiYears, 467)),
    ("北魏孝文帝延興", "八月", "", "", "", (BeiWeiYears, 471)),
    ("北魏孝文帝承明", "六月", "", "", "", (BeiWeiYears, 476)),
    ("北魏孝文帝太和", "", "", "", "", (BeiWeiYears, 477)),
    // TODO: remove "end"    
    ("北魏宣武帝景明", "", "八月", "", "", (BeiWeiYears, 500))
    // ("北魏宣武帝正始", "", "", "", "", (BeiWeiYears, 504)),
    // ("北魏宣武帝永平", "八月", "", "", "", (BeiWeiYears, 508)),
    // ("北魏宣武帝延昌", "四月", "", "", "", (BeiWeiYears, 512)),
    // ("北魏孝明帝熙平", "", "", "", "", (BeiWeiYears, 516)),
    // ("北魏孝明帝神龜", "二月", "", "", "", (BeiWeiYears, 518)),
    // ("北魏孝明帝正光", "七月", "", "", "", (BeiWeiYears, 520)),
    // ("北魏孝明帝孝昌", "六月", "", "", "", (BeiWeiYears, 525)),
    // ("北魏孝明帝武泰", "", "", "", "", (BeiWeiYears, 528)),
    // ("北魏孝莊帝建義", "四月", "", "", "", (BeiWeiYears, 528)),
    // ("北魏孝莊帝永安", "九月", "", "", "", (BeiWeiYears, 528)),
    // ("北魏長廣王建明", "十月", "", "", "", (BeiWeiYears, 530)),
    // ("北魏節閔帝普泰", "二月", "", "", "", (BeiWeiYears, 531)),
    // ("北魏安定王中興", "十月", "", "", "", (BeiWeiYears, 531)),
    // ("北魏孝武帝太昌", "四月", "", "", "", (BeiWeiYears, 532)),
    // ("北魏孝武帝永興", "十二月", "", "", "", (BeiWeiYears, 532)),
    // // TODO: remove "end", add "next"
    // // TODO: add test
    // ("北魏孝武帝永熙", "十二月", "三年", "", "", (BeiWeiYears, 532))
   )

  private var eraMap = new mutable.HashMap[String, (Array[Year], Int)]()
  for (era <- eraArray) {
    val (eraName, _, _, _, _, info) = era
    if (eraName != "") {
      eraMap(eraName) = info
    }
  }

  setDateFirstDayBCE()

  setMonthLength(BCEYears)
  setMonthLength(CEYears)
  setMonthLength(ShuYears)
  setMonthLength(WuYears)
  setMonthLength(BeiWeiYears)

  setMonthLengthLastYear(BCEYears, 29)  
  // Value 30 is tentatively set.
  setMonthLengthLastYear(ShuYears, 30)
  // No need for WuYears as the last year is shared with Wei.
  // TODO:
  //setMonthLengthLastYear(BeiWeiYears, 30)

  setSexagenary("辛亥", BCEYears)
  setSexagenary("辛酉", CEYears)
  setSexagenary("癸卯", ShuYears)
  setSexagenary("壬寅", WuYears)
  setSexagenary("庚辰", BeiWeiYears)  

  private var eraSegmentArray = new Array[EraSegment](eraArray.length)
  private val eraDurationMap = new mutable.HashMap[String, List[EraSegment]]()
  private var eraPartitionArray: Array[Int] = null

  processEraArray()

  private val FirstDay = eraSegmentArray(0).start
  private val LastDay = eraSegmentArray(eraSegmentArray.length - 1).end

  buildPrediction()
}
