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
  // 干支
  private val Sexagenary = Array(
    "甲子", "乙丑", "丙寅", "丁卯", "戊辰", "己巳", "庚午", "辛未", "壬申", "癸酉",
    "甲戌", "乙亥", "丙子", "丁丑", "戊寅", "己卯", "庚辰", "辛巳", "壬午", "癸未",
    "甲申", "乙酉", "丙戌", "丁亥", "戊子", "己丑", "庚寅", "辛卯", "壬辰", "癸巳",
    "甲午", "乙未", "丙申", "丁酉", "戊戌", "己亥", "庚子", "辛丑", "壬寅", "癸卯",
    "甲辰", "乙巳", "丙午", "丁未", "戊申", "己酉", "庚戌", "辛亥", "壬子", "癸丑",
    "甲寅", "乙卯", "丙辰", "丁巳", "戊午", "己未", "庚申", "辛酉", "壬戌", "癸亥"
  )

  def toGregorianCalendar(date: ChineseDate): GregorianCalendar = {
    val year = date.year.dropRight(1)   // Remove 年
    val yearOffset = Numbers.indexOf(year) - 1
    val monarchEra = date.monarchEra
    val Year(firstDay, months) = eraMap(monarchEra) match {
      case ((start, table), ad) => table(ad - start + yearOffset)
    }

    val Some(Month(_, sexagenary)) = months.find(_.month == date.month)

    firstDay
  }

  def toGregorianCalendar(date: String): GregorianCalendar =
    toGregorianCalendar(parseDate(date))

  def fromGregorianCalendar(calendar: GregorianCalendar): String = {
    ""
  }

  /** 
    * All fields are in Traditional Chinese. 
    * @param monarchEra This could be either the title of the monarch, or the era (年號), or both.
    *                   For monarchs with era (except for 漢武帝 who
    *                   firstly used era, but also ruled without era
    *                   before the first usage), usage of monarch only
    *                   is not allowed. When only use era, it should
    *                   be unique. When use both, they are concated together like 漢武帝建元.
    */
  case class ChineseDate(monarchEra: String, year: String, 
                         month: String, dayOfMonth: String) {
    override def toString = monarchEra + year + month + dayOfMonth
  }

  private def parseDate(s: String): ChineseDate = {
    var dayOfMonth = ""
    var endIndex = s.length
    if (s.endsWith("日")) {
      val k = s.lastIndexOf("月")
      assert(k != -1)
      endIndex = k + 1
      dayOfMonth = s.substring(k + 1)
    }

    parseMonth(s.substring(0, endIndex), dayOfMonth)
  }

  private def parseMonth(s: String, dayOfMonth: String): ChineseDate = {
    var month = ""
    var endIndex = s.length
    if (s.endsWith("月")) {
      val k = s.lastIndexOf("年")
      assert(k != -1)
      endIndex = k + 1
      month = s.substring(k + 1)
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
  private val LeapMonth = "閏"

  private val MonthInts = Array(-1, Calendar.JANUARY, Calendar.FEBRUARY)

  /** Return a data (as in Gregorian Calendar) given year, month (only
    * January and February), and date. */
  private def date(year: Int, month: Int, dayOfMonth: Int) =
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
  // The first element is the start year for the first entry in the corresponding array.
  private val ADYears = (1, Array(
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
    y(34, 2, 8,  ""), 
    y(35, 1, 28, ""),
    y(36, 2, 99, ""), 
    y(37, 2, 99, ""),
    y(38, 2, 99, ""), 
    y(39, 2, 99, ""),
    y(40, 2, 99, ""), 
    y(41, 2, 99, ""),
    y(42, 2, 99, ""), 
    y(43, 2, 99, ""),
    y(44, 2, 99, ""), 
    y(45, 2, 99, ""),
    y(46, 2, 99, ""), 
    y(47, 2, 99, ""),
    y(48, 2, 99, ""), 
    y(49, 2, 99, ""),
    y(50, 2, 99, ""), 
    y(51, 2, 99, ""),
    y(52, 2, 99, ""), 
    y(53, 2, 99, ""),
    y(54, 2, 99, ""), 
    y(55, 2, 99, ""),
    y(56, 2, 99, ""), 
    y(57, 2, 99, ""),
    y(58, 2, 99, ""), 
    y(59, 2, 99, ""),
    y(60, 2, 99, ""),
    y(61, 2, 99, ""),
    y(62, 2, 99, ""), 
    y(63, 2, 99, ""),
    y(64, 2, 99, ""), 
    y(65, 2, 99, ""),
    y(66, 2, 99, ""), 
    y(67, 2, 99, ""),
    y(68, 2, 99, ""), 
    y(69, 2, 99, ""),
    y(70, 2, 99, ""), 
    y(71, 2, 99, ""),
    y(72, 2, 99, ""), 
    y(73, 2, 99, ""),
    y(74, 2, 99, ""), 
    y(75, 2, 99, ""),
    y(76, 2, 99, ""), 
    y(77, 2, 99, ""),
    y(78, 2, 99, ""), 
    y(79, 2, 99, ""),
    y(80, 2, 99, ""),
    y(81, 2, 99, ""),
    y(82, 2, 99, ""), 
    y(83, 2, 99, ""),
    y(84, 2, 99, ""), 
    y(85, 2, 99, ""),
    y(86, 2, 99, ""), 
    y(87, 2, 99, ""),
    y(88, 2, 99, ""), 
    y(89, 2, 99, ""),
    y(90, 2, 99, ""), 
    y(91, 2, 99, ""),
    y(92, 2, 99, ""), 
    y(93, 2, 99, ""),
    y(94, 2, 99, ""), 
    y(95, 2, 99, ""),
    y(96, 2, 99, ""), 
    y(97, 2, 99, ""),
    y(98, 2, 99, ""), 
    y(99, 2, 99, ""),
    y(100, 2, 99, ""),
    y(101, 2, 99, ""),
    y(102, 2, 99, ""), 
    y(103, 2, 99, ""),
    y(104, 2, 99, ""), 
    y(105, 2, 99, ""),
    y(106, 2, 99, ""), 
    y(107, 2, 99, ""),
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
  ))
  private val ShuYears = (223, Array(
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
  ))
  private val WuYears = (222, Array(
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
  ))  

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
    (List("漢安帝延光", "延光"), (ADYears, 122))
  )
  private var eraMap = new mutable.HashMap[String, ((Int, Array[Year]), Int)]()
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
}
