/**
 * Chinese Calendar.
 *
 * @author  Yujian Zhang <yujian{dot}zhang[at]gmail(dot)com>
 *
 * License: 
 *   GNU General Public License v2
 *   http://www.gnu.org/licenses/gpl-2.0.html
 * Copyright (C) 2014 Yujian Zhang
 */

package net.whily.chinesecalendar

import java.util.{Calendar, GregorianCalendar}
import scala.collection.mutable    // For toMap()

object ChineseCalendar {
  // For debugging purpose only. Will be removed once code is stable.
  def main(args: Array[String]) {
    toGregorianCalendar("元始元年三月廿一")
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

  def toGregorianCalendar(date: ChineseDate): GregorianCalendar = {
    val year = date.year.dropRight(1)   // Remove 年
    val yearOffset = Numbers.indexOf(year) - 1
    val monarchEra = date.monarchEra

    val Year(firstDay, months) = eraMap(monarchEra) match {
      case (table, ad) =>
        val start = table(0).firstDay.get(Calendar.YEAR) // Year of the 1st entry in the table.
        table(ad - start + yearOffset)
    }

    val (dayDiff, sexagenary) = daysFromNewYear(date.month, months)
    val dayOfMonth = date.dayOfMonth
    val deltaDiff =
      if (Sexagenary.contains(dayOfMonth)) sexagenaryDiff(sexagenary, dayOfMonth)
      else Date.indexOf(dayOfMonth)

    // Since we modify the day, we need to clone it as the table can
    // be read multiple times.
    val day = clone(firstDay)
    day.add(Calendar.DAY_OF_MONTH, dayDiff + deltaDiff)
    day
  }

  private def clone(date: GregorianCalendar) =
    new GregorianCalendar(
      date.get(Calendar.YEAR),
      date.get(Calendar.MONTH),
      date.get(Calendar.DAY_OF_MONTH))

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
      assert((diff == 0) || (diff == 29) || (diff == 30))

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
    *   ChineseDate = monarchEra [year [month [dayOfMonth]]]
    * 
    * @param monarchEra This could be either the title of the monarch, or the era (年號), or both.
    *                   For monarchs with era (except for 漢武帝 who
    *                   firstly used era, but also ruled without era
    *                   before the first usage), usage of monarch only
    *                   is not allowed. When only use era, it should
    *                   be unique. When use both, they are concatenated together like 漢武帝建元.
    * @param year 元年|二年|三年|...
    * @param month the grammar is: month = [春|夏|秋|冬] [閏] 正月|一月|二月|三月|...|十月|十一月|十二月
    *        Note that whether the combination of season/month is valid or not is not checked.
    * @param dayOfMonth the grammar is:
    *        dayOfMonth = Sexagenary|朔|初一|初二|...|初九|初十|十一|十二|...|十九|二十|廿一|廿二|...|廿九|三十|
    */  
  def toGregorianCalendar(date: String): GregorianCalendar = {
    // An example of string with minimum length: 黃初元年
    assert(date.length >= 4)  
    toGregorianCalendar(parseDate(date))
  }

  def fromGregorianCalendar(calendar: GregorianCalendar): String = {
    ""
  }

  case class ChineseDate(monarchEra: String, year: String, 
                         month: String, dayOfMonth: String) {
    override def toString = monarchEra + year + month + dayOfMonth
  }

  private def parseDate(s: String): ChineseDate = {
    var dayOfMonth = "初一"   // Default day of month.
    var endIndex = s.length
    if (s.takeRight(1) == "朔") {
      dayOfMonth = "初一"
      endIndex -= 1
    } else {
      val t = s.takeRight(2)
      if (Sexagenary.contains(t) || Date.contains(t)) {
        endIndex -= 2
        val k = s.lastIndexOf("月")
        assert(k + 1 == endIndex)
        dayOfMonth = t
      }
    }

    parseMonth(s.substring(0, endIndex), dayOfMonth)
  }

  private def parseMonth(s: String, dayOfMonth: String): ChineseDate = {
    var month = "一月"  // Default month
    var endIndex = s.length
    if (s.endsWith("月")) {
      val k = s.lastIndexOf("年")
      assert(k != -1)
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

  private def parseYear(s: String, month: String, dayOfMonth: String): ChineseDate = {
    assert(s.takeRight(1) == "年")

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
        assert(Numbers.contains(year))
      }
    }

    val monarchEra = t.dropRight(year.length)
    assert(eraMap.contains(monarchEra))

    ChineseDate(monarchEra, year + "年", month, dayOfMonth)
  }

  /**
    * @param month       month name
    * @param sexagenary sexagenary (干支) of the first day of the month
    */
  case class Month(month: String, sexagenary: String) {
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
  private val Date = Array(
    "初一", "初二", "初三", "初四", "初五", "初六", "初七", "初八", "初九", "初十",
    "十一", "十二", "十三", "十四", "十五", "十六", "十七", "十八", "十九", "二十",
    "廿一", "廿二", "廿三", "廿四", "廿五", "廿六", "廿七", "廿八", "廿九", "三十"
  )
  private val LeapMonth = "閏"

  private val MonthInts = Array(-1, Calendar.JANUARY, Calendar.FEBRUARY, Calendar.MARCH,
    Calendar.APRIL, Calendar.MAY, Calendar.JUNE, Calendar.JULY,
    Calendar.AUGUST, Calendar.SEPTEMBER, Calendar.OCTOBER, Calendar.NOVEMBER, Calendar.DECEMBER)

  /** Return a data (as in Gregorian Calendar) given year, month (only
    * January and February), and date. */
  def date(year: Int, month: Int, dayOfMonth: Int) =
    new GregorianCalendar(year, MonthInts(month), dayOfMonth)

  /** Return an array of months by parsing the string S, in the format of
    *   sexageneray1 sexagenary2 ...
    * If there is a leap month, then insert character 閏.
    */
  def months(s: String): Array[Month] = {
    val words = s.trim.split(" ")
    var monthIndex = 1
    var result: List[Month] = Nil
    var prefix = ""
    for (word <- words) {
      if (word != LeapMonth) {
        if (prefix == LeapMonth)
          monthIndex -= 1

        result = Month(prefix + Numbers(monthIndex) + "月", word) :: result
        prefix = ""
        monthIndex += 1
      } else {
        prefix = LeapMonth
      }
    }
    result.reverse.toArray
  }

  /**
    * Represents one year in Chinese calendar. 
    * 
    * @param firstDay   the first day in Gregorian Calendar.
    * @param months     the 12 or 13 months in the year
    */
  private case class Year(firstDay: GregorianCalendar, months: Array[Month])

  /** Return object Year given year, month, dayOfMonth, months. */
  private def y(year: Int, month: Int, dayOfMonth: Int, monthStr: String) =
    Year(date(year, month, dayOfMonth), months(monthStr))

  // Information from 中国史历日和中西历日对照表 (方诗铭，方小芬 著)
  private val ADYears = Array(
    y(1,  2, 11, "己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申"), 
    y(2,  2, 1,  "甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 閏 庚戌 己卯 己酉 戊寅 戊申"),
    y(3,  2, 20, "戊寅 丁未 丁丑 丙午 丙子 乙巳 乙亥 甲辰 甲戌 癸卯 癸酉 壬寅"),
    y(4,  2, 9,  "壬申 辛丑 辛未 庚子 庚午 庚子 己巳 己亥 戊辰 戊戌 丁卯 丁酉"), 
    y(5,  1, 29, "丙寅 丙申 乙丑 乙未 甲子 閏 甲午 癸亥 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉"), 
    y(6,  2, 17, "庚寅 庚申 己丑 己未 戊子 戊午 丁亥 丁巳 丙戌 丙辰 乙酉 乙卯"),
    y(7,  2, 7,  "乙酉 甲寅 甲申 癸丑 癸未 壬子 壬午 辛亥 辛巳 庚戌 庚辰 己酉"),
    y(8,  1, 27, "己卯 閏 戊申 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉"), 
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
    y(81, 2, 17, "乙巳 乙亥 甲辰 甲戌 癸卯 閏 癸酉 壬寅 壬申 壬寅 辛未 辛丑 庚午 庚子"),
    y(82, 2, 7,  "己巳 己亥 戊辰 戊戌 丁卯 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午"), 
    y(83, 2, 99, "甲子 癸巳 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 戊子"),
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
    y(108, 2, 99, ""), 
    y(109, 2, 99, ""),
    y(110, 2, 99, ""), 
    y(111, 2, 99, ""),
    y(112, 2, 99, ""), 
    y(113, 2, 99, ""),
    y(114, 2, 99, ""), 
    y(115, 2, 99, ""),
    y(116, 2, 99, ""), 
    y(117, 2, 99, ""),
    y(118, 2, 99, ""), 
    y(119, 2, 99, ""),
    y(120, 2, 99, ""), 
    y(121, 2, 99, ""),
    y(122, 2, 99, ""), 
    y(123, 2, 99, ""),
    y(124, 2, 99, ""), 
    y(125, 2, 99, ""),
    y(126, 2, 99, ""), 
    y(127, 2, 99, ""),
    y(128, 2, 99, ""), 
    y(129, 2, 99, ""),
    y(130, 2, 99, ""), 
    y(131, 2, 99, ""),
    y(132, 2, 99, ""), 
    y(133, 2, 99, ""),
    y(134, 2, 99, ""), 
    y(135, 2, 99, ""),
    y(136, 2, 99, ""), 
    y(137, 2, 99, ""),
    y(138, 2, 99, ""), 
    y(139, 2, 99, ""),
    y(140, 2, 99, ""), 
    y(141, 2, 99, ""),
    y(142, 2, 99, ""), 
    y(143, 2, 99, ""),
    y(144, 2, 99, ""), 
    y(145, 2, 99, ""),
    y(146, 2, 99, ""), 
    y(147, 2, 99, ""),
    y(148, 2, 99, ""), 
    y(149, 2, 99, ""),
    y(150, 2, 99, ""), 
    y(151, 2, 99, ""),
    y(152, 2, 99, ""), 
    y(153, 2, 99, ""),
    y(154, 2, 99, ""), 
    y(155, 2, 99, ""),
    y(156, 2, 99, ""), 
    y(157, 2, 99, ""),
    y(158, 2, 99, ""), 
    y(159, 2, 99, ""),
    y(160, 2, 99, ""),
    y(161, 2, 99, ""),
    y(162, 2, 99, ""), 
    y(163, 2, 99, ""),
    y(164, 2, 99, ""), 
    y(165, 2, 99, ""),
    y(166, 2, 99, ""), 
    y(167, 2, 99, ""),
    y(168, 2, 99, ""), 
    y(169, 2, 99, ""),
    y(170, 2, 99, ""), 
    y(171, 2, 99, ""),
    y(172, 2, 99, ""), 
    y(173, 2, 99, ""),
    y(174, 2, 99, ""), 
    y(175, 2, 99, ""),
    y(176, 2, 99, ""), 
    y(177, 2, 99, ""),
    y(178, 2, 99, ""), 
    y(179, 2, 99, ""),
    y(180, 2, 99, ""),
    y(181, 2, 99, ""),
    y(182, 2, 99, ""), 
    y(183, 2, 99, ""),
    y(184, 2, 99, ""), 
    y(185, 2, 99, ""),
    y(186, 2, 99, ""), 
    y(187, 2, 99, ""),
    y(188, 2, 99, ""), 
    y(189, 2, 99, ""),
    y(190, 2, 99, ""), 
    y(191, 2, 99, ""),
    y(192, 2, 99, ""), 
    y(193, 2, 99, ""),
    y(194, 2, 99, ""), 
    y(195, 2, 99, ""),
    y(196, 2, 99, ""), 
    y(197, 2, 99, ""),
    y(198, 2, 99, ""), 
    y(199, 2, 99, ""),
    y(200, 2, 99, ""),
    y(201, 2, 99, ""),
    y(202, 2, 99, ""), 
    y(203, 2, 99, ""),
    y(204, 2, 99, ""), 
    y(205, 2, 99, ""),
    y(206, 2, 99, ""), 
    y(207, 2, 99, ""),
    y(208, 2, 99, ""), 
    y(209, 2, 99, ""),
    y(210, 2, 99, ""), 
    y(211, 2, 99, ""),
    y(212, 2, 99, ""), 
    y(213, 2, 99, ""),
    y(214, 2, 99, ""), 
    y(215, 2, 99, ""),
    y(216, 2, 99, ""), 
    y(217, 2, 99, ""),
    y(218, 2, 99, ""), 
    y(219, 2, 99, ""),
    y(220, 2, 99, ""), 
    y(221, 2, 99, ""),
    y(222, 2, 99, ""), 
    y(223, 2, 99, ""),
    y(224, 2, 99, ""), 
    y(225, 2, 99, ""),
    y(226, 2, 99, ""), 
    y(227, 2, 99, ""),
    y(228, 2, 99, ""), 
    y(229, 2, 99, ""),
    y(230, 2, 99, ""), 
    y(231, 2, 99, ""),
    y(232, 2, 99, ""), 
    y(233, 2, 99, ""),
    y(234, 2, 99, ""), 
    y(235, 2, 99, ""),
    y(236, 2, 99, ""), 
    y(237, 2, 99, ""),
    y(238, 2, 99, ""), 
    y(239, 2, 99, ""),
    y(240, 2, 99, ""), 
    y(241, 2, 99, ""),
    y(242, 2, 99, ""), 
    y(243, 2, 99, ""),
    y(244, 2, 99, ""), 
    y(245, 2, 99, ""),
    y(246, 2, 99, ""), 
    y(247, 2, 99, ""),
    y(248, 2, 99, ""), 
    y(249, 2, 99, ""),
    y(250, 2, 99, ""), 
    y(251, 2, 99, ""),
    y(252, 2, 99, ""), 
    y(253, 2, 99, ""),
    y(254, 2, 99, ""), 
    y(255, 2, 99, ""),
    y(256, 2, 99, ""), 
    y(257, 2, 99, ""),
    y(258, 2, 99, ""), 
    y(259, 2, 99, ""),
    y(260, 2, 99, ""),
    y(261, 2, 99, ""),
    y(262, 2, 99, ""), 
    y(263, 2, 99, ""),
    y(264, 2, 99, ""), 
    y(265, 2, 99, ""),
    y(266, 2, 99, ""), 
    y(267, 2, 99, ""),
    y(268, 2, 99, ""), 
    y(269, 2, 99, ""),
    y(270, 2, 99, ""), 
    y(271, 2, 99, ""),
    y(272, 2, 99, ""), 
    y(273, 2, 99, ""),
    y(274, 2, 99, ""), 
    y(275, 2, 99, ""),
    y(276, 2, 99, ""), 
    y(277, 2, 99, ""),
    y(278, 2, 99, ""), 
    y(279, 2, 99, ""),
    y(280, 2, 99, ""),
    y(281, 2, 99, ""),
    y(282, 2, 99, ""), 
    y(283, 2, 99, ""),
    y(284, 2, 99, ""), 
    y(285, 2, 99, ""),
    y(286, 2, 99, ""), 
    y(287, 2, 99, ""),
    y(288, 2, 99, ""), 
    y(289, 2, 99, ""),
    y(290, 2, 99, ""), 
    y(291, 2, 99, ""),
    y(292, 2, 99, ""), 
    y(293, 2, 99, ""),
    y(294, 2, 99, ""), 
    y(295, 2, 99, ""),
    y(296, 2, 99, ""), 
    y(297, 2, 99, ""),
    y(298, 2, 99, ""), 
    y(299, 2, 99, ""),
    y(300, 2, 99, ""),
    y(301, 2, 99, ""),
    y(302, 2, 99, ""), 
    y(303, 2, 99, ""),
    y(304, 2, 99, ""), 
    y(305, 2, 99, ""),
    y(306, 2, 99, ""), 
    y(307, 2, 99, ""),
    y(308, 2, 99, ""), 
    y(309, 2, 99, ""),
    y(310, 2, 99, ""), 
    y(311, 2, 99, ""),
    y(312, 2, 99, ""), 
    y(313, 2, 99, ""),
    y(314, 2, 99, ""), 
    y(315, 2, 99, ""),
    y(316, 2, 99, ""), 
    y(317, 2, 99, ""),
    y(318, 2, 99, ""), 
    y(319, 2, 99, ""),
    y(320, 2, 99, ""), 
    y(321, 2, 99, ""),
    y(322, 2, 99, ""), 
    y(323, 2, 99, ""),
    y(324, 2, 99, ""), 
    y(325, 2, 99, ""),
    y(326, 2, 99, ""), 
    y(327, 2, 99, ""),
    y(328, 2, 99, ""), 
    y(329, 2, 99, ""),
    y(330, 2, 99, ""), 
    y(331, 2, 99, ""),
    y(332, 2, 99, ""), 
    y(333, 2, 99, ""),
    y(334, 2, 99, ""), 
    y(335, 2, 99, ""),
    y(336, 2, 99, ""), 
    y(337, 2, 99, ""),
    y(338, 2, 99, ""), 
    y(339, 2, 99, ""),
    y(340, 2, 99, ""), 
    y(341, 2, 99, ""),
    y(342, 2, 99, ""), 
    y(343, 2, 99, ""),
    y(344, 2, 99, ""), 
    y(345, 2, 99, ""),
    y(346, 2, 99, ""), 
    y(347, 2, 99, ""),
    y(348, 2, 99, ""), 
    y(349, 2, 99, ""),
    y(350, 2, 99, ""), 
    y(351, 2, 99, ""),
    y(352, 2, 99, ""), 
    y(353, 2, 99, ""),
    y(354, 2, 99, ""), 
    y(355, 2, 99, ""),
    y(356, 2, 99, ""), 
    y(357, 2, 99, ""),
    y(358, 2, 99, ""), 
    y(359, 2, 99, ""),
    y(360, 2, 99, ""),
    y(361, 2, 99, ""),
    y(362, 2, 99, ""), 
    y(363, 2, 99, ""),
    y(364, 2, 99, ""), 
    y(365, 2, 99, ""),
    y(366, 2, 99, ""), 
    y(367, 2, 99, ""),
    y(368, 2, 99, ""), 
    y(369, 2, 99, ""),
    y(370, 2, 99, ""), 
    y(371, 2, 99, ""),
    y(372, 2, 99, ""), 
    y(373, 2, 99, ""),
    y(374, 2, 99, ""), 
    y(375, 2, 99, ""),
    y(376, 2, 99, ""), 
    y(377, 2, 99, ""),
    y(378, 2, 99, ""), 
    y(379, 2, 99, ""),
    y(380, 2, 99, ""),
    y(381, 2, 99, ""),
    y(382, 2, 99, ""), 
    y(383, 2, 99, ""),
    y(384, 2, 99, ""), 
    y(385, 2, 99, ""),
    y(386, 2, 99, ""), 
    y(387, 2, 99, ""),
    y(388, 2, 99, ""), 
    y(389, 2, 99, ""),
    y(390, 2, 99, ""), 
    y(391, 2, 99, ""),
    y(392, 2, 99, ""), 
    y(393, 2, 99, ""),
    y(394, 2, 99, ""), 
    y(395, 2, 99, ""),
    y(396, 2, 99, ""), 
    y(397, 2, 99, ""),
    y(398, 2, 99, ""), 
    y(399, 2, 99, "")
  )
  private val ShuYears = Array(
    y(223, 2, 99, ""),
    y(224, 2, 99, ""), 
    y(225, 2, 99, ""),
    y(226, 2, 99, ""), 
    y(227, 2, 99, ""),
    y(228, 2, 99, ""), 
    y(229, 2, 99, ""),
    y(230, 2, 99, ""), 
    y(231, 2, 99, ""),
    y(232, 2, 99, ""), 
    y(233, 2, 99, ""),
    y(234, 2, 99, ""), 
    y(235, 2, 99, ""),
    y(236, 2, 99, ""), 
    y(237, 2, 99, ""),
    y(238, 2, 99, ""), 
    y(239, 2, 99, ""),
    y(240, 2, 99, ""), 
    y(241, 2, 99, ""),
    y(242, 2, 99, ""), 
    y(243, 2, 99, ""),
    y(244, 2, 99, ""), 
    y(245, 2, 99, ""),
    y(246, 2, 99, ""), 
    y(247, 2, 99, ""),
    y(248, 2, 99, ""), 
    y(249, 2, 99, ""),
    y(250, 2, 99, ""), 
    y(251, 2, 99, ""),
    y(252, 2, 99, ""), 
    y(253, 2, 99, ""),
    y(254, 2, 99, ""), 
    y(255, 2, 99, ""),
    y(256, 2, 99, ""), 
    y(257, 2, 99, ""),
    y(258, 2, 99, ""), 
    y(259, 2, 99, ""),
    y(260, 2, 99, ""),
    y(261, 2, 99, ""),
    y(262, 2, 99, ""), 
    y(263, 2, 99, ""),
    y(264, 2, 99, ""), 
    y(265, 2, 99, ""),
    y(266, 2, 99, ""), 
    y(267, 2, 99, ""),
    y(268, 2, 99, ""), 
    y(269, 2, 99, ""),
    y(270, 2, 99, ""), 
    y(271, 2, 99, ""),
    y(272, 2, 99, ""), 
    y(273, 2, 99, ""),
    y(274, 2, 99, ""), 
    y(275, 2, 99, ""),
    y(276, 2, 99, ""), 
    y(277, 2, 99, ""),
    y(278, 2, 99, ""), 
    y(279, 2, 99, ""),
    y(280, 2, 99, "")
  )
  private val WuYears = Array(
    y(222, 2, 99, ""), 
    y(223, 2, 99, ""),
    y(224, 2, 99, ""), 
    y(225, 2, 99, ""),
    y(226, 2, 99, ""), 
    y(227, 2, 99, ""),
    y(228, 2, 99, ""), 
    y(229, 2, 99, ""),
    y(230, 2, 99, ""), 
    y(231, 2, 99, ""),
    y(232, 2, 99, ""), 
    y(233, 2, 99, ""),
    y(234, 2, 99, ""), 
    y(235, 2, 99, ""),
    y(236, 2, 99, ""), 
    y(237, 2, 99, ""),
    y(238, 2, 99, ""), 
    y(239, 2, 99, ""),
    y(240, 2, 99, ""), 
    y(241, 2, 99, ""),
    y(242, 2, 99, ""), 
    y(243, 2, 99, ""),
    y(244, 2, 99, ""), 
    y(245, 2, 99, ""),
    y(246, 2, 99, ""), 
    y(247, 2, 99, ""),
    y(248, 2, 99, ""), 
    y(249, 2, 99, ""),
    y(250, 2, 99, ""), 
    y(251, 2, 99, ""),
    y(252, 2, 99, ""), 
    y(253, 2, 99, ""),
    y(254, 2, 99, ""), 
    y(255, 2, 99, ""),
    y(256, 2, 99, ""), 
    y(257, 2, 99, ""),
    y(258, 2, 99, ""), 
    y(259, 2, 99, ""),
    y(260, 2, 99, ""),
    y(261, 2, 99, ""),
    y(262, 2, 99, ""), 
    y(263, 2, 99, "")
  )  

  // The 1st element of the value is the corresponding table, while
  // the 2nd element is the start year in Gregorian Calendar.
  private val eraList = List(
    (List("漢平帝元始", "元始"), (ADYears, 1)),
    (List("漢孺子嬰居攝", "居攝"), (ADYears, 6)),
    (List("漢孺子嬰初始", "初始"), (ADYears, 8)),
    (List("新王莽始建國", "始建國"), (ADYears, 9)),
    (List("新王莽天鳳", "天鳳") , (ADYears, 14)),
    (List("新王莽地皇", "地皇"), (ADYears, 20)),
    (List("劉玄更始", "更始"), (ADYears, 23)),
    (List("漢光武帝建武"), (ADYears, 25)),
    (List("漢光武帝建武中元", "建武中元"), (ADYears, 56)),
    (List("漢明帝永平"), (ADYears, 58)),
    (List("漢章帝建初", "建初"), (ADYears, 76)),
    (List("漢章帝元和"), (ADYears, 84)),
    (List("漢章帝章和", "章和"), (ADYears, 87)),
    (List("漢和帝永元"), (ADYears, 89)),
    (List("漢和帝元興"), (ADYears, 105)),
    (List("漢殤帝延平", "延平"), (ADYears, 106)),
    (List("漢安帝永初"), (ADYears, 107)),
    (List("漢安帝元初", "元初"), (ADYears, 114)),
    (List("漢安帝永寧"), (ADYears, 120)),
    (List("漢安帝建光", "建光"), (ADYears, 121)),
    (List("漢安帝延光", "延光"), (ADYears, 122)),
    (List("漢順帝永建"), (ADYears, 126)),
    (List("漢順帝陽嘉", "陽嘉"), (ADYears, 132)),
    (List("漢順帝永和"), (ADYears, 136)),
    (List("漢順帝漢安", "漢安"), (ADYears, 142)),
    (List("漢順帝建康"), (ADYears, 144)),
    (List("漢沖帝永憙", "永憙"), (ADYears, 145)),
    (List("漢質帝本初", "本初"), (ADYears, 146)),
    (List("漢桓帝建和"), (ADYears, 147)),
    (List("漢桓帝和平"), (ADYears, 150)),
    (List("漢桓帝元嘉"), (ADYears, 151)),
    (List("漢桓帝永興"), (ADYears, 153)),
    (List("漢桓帝永壽", "永壽"), (ADYears, 155)),
    (List("漢桓帝延熹", "延熹"), (ADYears, 158)),
    (List("漢桓帝永康"), (ADYears, 167)),
    (List("漢靈帝建寧", "建寧"), (ADYears, 168)),
    (List("漢靈帝熹平", "熹平"), (ADYears, 172)),
    (List("漢靈帝光和", "光和"), (ADYears, 178)),
    (List("漢靈帝中平", "中平", "漢獻帝中平"), (ADYears, 184)),
    (List("漢少帝光熹", "光熹", "漢少帝昭寧", "昭寧", "漢獻帝永漢"), (ADYears, 189)),
    (List("漢獻帝初平", "初平"), (ADYears, 190)),
    (List("漢獻帝興平"), (ADYears, 194)),
    (List("漢獻帝建安"), (ADYears, 196)),  // Also used by 段正明, although exact duration unknown.
    (List("漢獻帝延康"), (ADYears, 220)),
    (List("魏文帝黃初", "黃初"), (ADYears, 220)),
    (List("魏明帝太和"), (ADYears, 227)),
    (List("魏明帝青龍"), (ADYears, 233)),
    (List("魏明帝景初", "景初"), (ADYears, 237)),
    (List("魏齊王芳正始"), (ADYears, 240)),
    (List("魏齊王芳嘉平"), (ADYears, 249)),
    (List("魏高貴鄉公正元", "正元"), (ADYears, 254)),
    (List("魏高貴鄉公甘露"), (ADYears, 256)),
    (List("魏陳留王景元", "景元"), (ADYears, 260)),
    (List("魏陳留王咸熙", "咸熙"), (ADYears, 264)),
    (List("晉武帝泰始"), (ADYears, 265)),
    (List("晉武帝咸寧"), (ADYears, 275)),
    (List("晉武帝太康"), (ADYears, 280)),
    (List("晉武帝太熙", "太熙", "晉惠帝永熙"), (ADYears, 290)),
    (List("晉惠帝永平", "晉惠帝元康"), (ADYears, 291)),
    (List("晉惠帝永康"), (ADYears, 300)),
    (List("晉惠帝永寧"), (ADYears, 301)),
    (List("晉惠帝太安"), (ADYears, 302)),
    (List("晉惠帝永安", "晉惠帝建武", "晉惠帝永興"), (ADYears, 304)),
    (List("晉惠帝光熙", "光熙"), (ADYears, 306)),
    (List("晉懷帝永嘉"), (ADYears, 307)),
    (List("晉愍帝建興"), (ADYears, 313)),
    (List("晉元帝建武"), (ADYears, 317)),
    (List("晉元帝大興"), (ADYears, 318)),
    (List("晉元帝永昌"), (ADYears, 322)),
    (List("晉明帝太寧"), (ADYears, 323)),
    (List("晉成帝咸和"), (ADYears, 326)),
    (List("晉成帝咸康"), (ADYears, 335)),
    (List("晉康帝建元"), (ADYears, 343)),
    (List("晉穆帝永和"), (ADYears, 345)),
    (List("晉穆帝昇平"), (ADYears, 357)),
    (List("晉哀帝隆和", "隆和"), (ADYears, 362)),
    (List("晉哀帝興寧", "興寧"), (ADYears, 363)),
    (List("晉廢帝太和"), (ADYears, 366)),
    (List("晉簡文帝咸安", "咸安"), (ADYears, 371)),
    (List("晉孝武帝寧康", "寧康"), (ADYears, 373)),
    (List("晉孝武帝太元"), (ADYears, 376)),
    (List("晉安帝隆安", "隆安"), (ADYears, 397)),
    (List("晉安帝元興", "晉安帝大亨", "大亨"), (ADYears, 402)),
    (List("晉安帝義熙"), (ADYears, 405)),
    (List("晉恭帝元熙"), (ADYears, 419)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122)),      
    (List("", ""), (ADYears, 122)),
    (List("", ""), (ADYears, 122))    
  )
  private var eraMap = new mutable.HashMap[String, (Array[Year], Int)]()
  for (era <- eraList) {
    val (list, info) = era
    for (e <- list) {
      eraMap(e) = info
    }
  }

  // Create a map from the array, with each value mapped to its index.
  def toMap(a: Array[String]) = {
    var map = new mutable.HashMap[String, Int]()
    for (i <- 0 to a.length - 1) {
      map(a(i)) = i
    }
    map
  }

  // Check sanity of year tables.
  private def checkYearTable(table: Array[Year]): Boolean = {
    // Calculate the first day of next year based on the first day of
    // current year and the sum of the sexagenary diffenrence of the
    // months.
    var calculatedFirstDay = clone(table(0).firstDay)
    // The following value is selected to make the code also work for
    // the first year in the table.
    var prevSexagenary = table(0).months(0).sexagenary

    for (year <- table) {
      val Year(firstDay, months) = year
      if (months.length > 0) { // Ignore placeholders.
        calculatedFirstDay.add(Calendar.DAY_OF_MONTH,
          sexagenaryDiff(prevSexagenary, months(0).sexagenary))
        if (firstDay != calculatedFirstDay) {
          print(firstDay)
          return false
        }

        val (dayDiff, sexagenary) = daysFromNewYear(months.last.month, months)
        calculatedFirstDay.add(Calendar.DAY_OF_MONTH, dayDiff)
        prevSexagenary = sexagenary
      }
    }

    true
  }

  // Return a string which represents a pretty print version of Gregorian calendar.
  private def ppCalendar(date: GregorianCalendar) {
    (date.get(Calendar.YEAR) + 1).toString + "-" +
      (date.get(Calendar.MONTH) + 1).toString + "-" +
      (date.get(Calendar.DAY_OF_MONTH) + 1).toString
  }

  // Regresssion test to ensure the data tables are correct. Made
  // public so this can be called from as regression test.
  def sanityCheck: Boolean = {
    checkYearTable(ADYears) &&
    checkYearTable(ShuYears) &&
    checkYearTable(WuYears)
  }
}
