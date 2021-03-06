/**
 * Test cases for ChineseCalendar.scala.
 *
 * @author  Yujian Zhang <yujian{dot}zhang[at]gmail(dot)com>
 *
 * License:
 *   GNU General Public License v2
 *   http://www.gnu.org/licenses/gpl-2.0.html
 * Copyright (C) 2014 Yujian Zhang
 */

import net.whily.chinesecalendar.ChineseCalendar._
import net.whily.chinesecalendar.Chinese._
import org.scalatest._

class ChineseCalendarTest extends FunSpec with Matchers {
  describe("Test Chinese Calendar calculation") {
    it("Check months()") {
      months("己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申") should === (Array(
        Month("一月", "己未"), Month("二月", "己丑"), Month("三月", "戊午"),
        Month("四月", "戊子"), Month("五月", "丁巳"), Month("六月", "丁亥"),
        Month("七月", "丙辰"), Month("八月", "丙戌"), Month("九月", "丙辰"),
        Month("十月", "乙酉"), Month("十一月", "乙卯"), Month("十二月", "甲申")))
      months("甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 閏 庚戌 己卯 己酉 戊寅 戊申") should === (Array(
        Month("一月", "甲寅"), Month("二月", "癸未"), Month("三月", "癸丑"),
        Month("四月", "壬午"), Month("五月", "壬子"), Month("六月", "辛巳"),
        Month("七月", "辛亥"), Month("八月", "庚辰"), Month("閏八月", "庚戌"),
        Month("九月", "己卯"), Month("十月", "己酉"), Month("十一月", "戊寅"),
        Month("十二月", "戊申")))
      months("己卯 閏 戊申 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉") should === (Array(
        Month("一月", "己卯"), Month("閏一月", "戊申"), Month("二月", "戊寅"),
        Month("三月", "戊申"), Month("四月", "丁丑"), Month("五月", "丁未"),
        Month("六月", "丙子"), Month("七月", "丙午"), Month("八月", "乙亥"),
        Month("九月", "乙巳"), Month("十月", "甲戌"), Month("十一月", "甲辰"),
        Month("十二月", "癸酉")))
      months("己亥 己巳 進 戊戌 戊辰 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午") should === (Array(
        Month("一月", "己亥"), Month("二月", "己巳"), Month("四月", "戊戌"),
        Month("五月", "戊辰"), Month("六月", "丁酉"), Month("七月", "丙寅"),
        Month("八月", "丙申"), Month("九月", "乙丑"), Month("十月", "乙未"),
        Month("十一月", "甲子"), Month("十二月", "甲午")))
      months("丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 後 壬午") should === (Array(
        Month("一月", "丁亥"), Month("二月", "丁巳"), Month("三月", "丙戌"),
        Month("四月", "丙辰"), Month("五月", "乙酉"), Month("六月", "乙卯"),
        Month("七月", "甲申"), Month("八月", "甲寅"), Month("九月", "癸未"),
        Month("十月", "癸丑"), Month("十一月", "壬午"), Month("十二月", "壬子"),
        Month("後十二月", "壬午")))
      months("乙未 甲子 甲午 癸亥 壬辰 壬戌 辛卯 辛酉 庚寅 庚申 己丑 己未 後 戊子", 10) should === (Array(
        Month("十月", "乙未"), Month("十一月", "甲子"), Month("十二月", "甲午"),
        Month("一月", "癸亥"), Month("二月", "壬辰"), Month("三月", "壬戌"),
        Month("四月", "辛卯"), Month("五月", "辛酉"), Month("六月", "庚寅"),
        Month("七月", "庚申"), Month("八月", "己丑"), Month("九月", "己未"),
        Month("後九月", "戊子")))
      months("庚辰 己酉 己卯 戊申 戊寅 丁未 丙子 丙午 乙亥 甲辰 甲戌 甲辰", 1, "武周") should === (Array(
        Month("正月", "庚辰"), Month("臘月", "己酉"), Month("一月", "己卯"),
        Month("二月", "戊申"), Month("三月", "戊寅"), Month("四月", "丁未"),
        Month("五月", "丙子"), Month("六月", "丙午"), Month("七月", "乙亥"),
        Month("八月", "甲辰"), Month("九月", "甲戌"), Month("十月", "甲辰")))
      months("壬午 辛亥 辛巳 辛亥 庚辰 庚戌 己卯 己酉 戊寅 丁未 丁丑 丙午 丙子 乙巳", 5, "唐肅宗") should === (Array(
        Month("建子月", "壬午"), Month("建丑月", "辛亥"), Month("建寅月", "辛巳"),
        Month("建卯月", "辛亥"), Month("建辰月", "庚辰"), Month("建巳月", "庚戌"),
        Month("五月", "己卯"), Month("六月", "己酉"), Month("七月", "戊寅"),
        Month("八月", "丁未"), Month("九月", "丁丑"), Month("十月", "丙午"),
        Month("十一月", "丙子"), Month("十二月", "乙巳")))
    }

    it("Check parseDate().") {
      parseDate("漢武帝元朔六年") should be (parseDate("漢武帝元朔六年十月初一"))
    }

    it("Check toDate().") {
      // Check dates according to 三千五百年历日天象 (张培瑜 著)
      toDate("周桓王元年") should be (date(-719, 12, 12))
      toDate("周赧王元年") should be (date(-314, 12, 6))
      toDate("周赧王四十五年") should be (date(-270, 11, 30))
      toDate("周赧王五十九年十一月") should be (date(-255, 10, 16))

      // Check dates according to 三千五百年历日天象 (张培瑜 著)
      toDate("魯隱公元年") should be (date(-721, 1,16))
      // 以下為《中國古代曆法》表3-1所列的《春秋》記載的含有明確日干支並經過驗證的日食。
      toDate("魯隱公三年二月己巳") should be (date(-719, 2, 22))
      toDate("魯桓公三年七月壬辰") should be (date(-708, 7, 17))
      toDate("魯莊公二十五年六月辛未") should be (date(-668, 5, 27))
      toDate("魯莊公二十六年十二月癸亥") should be (date(-667, 11, 10))
      toDate("魯莊公三十年九月庚午") should be (date(-663, 8, 28))
      toDate("魯僖公五年九月戊申") should be (date(-654, 8, 19))
      toDate("魯僖公十二年三月庚午") should be (date(-647, 4, 6))
      toDate("魯文公元年二月癸亥") should be (date(-625, 2, 3))
      toDate("魯文公十五年六月辛丑") should be (date(-611, 4, 28))
      toDate("魯宣公十年四月丙辰") should be (date(-598, 3, 6))
      toDate("魯成公十六年六月丙寅") should be (date(-574, 5, 9))
      toDate("魯成公十七年十二月丁巳") should be (date(-573, 10, 22))
      toDate("魯襄公十四年二月乙未") should be (date(-558, 1, 14))
      toDate("魯襄公二十一年九月庚戌") should be (date(-551, 8, 20))
      toDate("魯襄公二十三年二月癸酉") should be (date(-549, 1, 5))
      toDate("魯襄公二十四年七月甲子") should be (date(-548, 6, 19))
      toDate("魯襄公二十七年十二月乙亥") should be (date(-545, 10, 13))
      toDate("魯昭公七年四月甲辰") should be (date(-534, 3, 18))
      toDate("魯昭公十五年六月丁巳") should be (date(-526, 4, 18))
      toDate("魯昭公二十一年七月壬午") should be (date(-520, 6, 10))
      toDate("魯昭公二十二年十二月癸酉") should be (date(-519, 11, 23))
      toDate("魯昭公二十四年五月乙未") should be (date(-517, 4, 9))
      toDate("魯昭公三十一年十二月辛亥") should be (date(-510, 11, 14))
      toDate("魯定公五年三月辛亥") should be (date(-504, 2, 16))
      toDate("魯定公十二年十一月丙寅") should be (date(-497, 9, 22))
      toDate("魯定公十五年八月庚辰") should be (date(-494, 7, 22))
      toDate("魯哀公十四年五月庚申") should be (date(-480, 4, 19))

      // Check dates according to 三千五百年历日天象 (张培瑜 著)
      toDate("秦昭襄王元年十一月") should be (date(-306, 12, 8))

      // TODO: 秦孝文王即位
      // TODO: toDate("秦孝文王元年十月己亥") should be (date(-250, 11, 12))
      toDate("秦孝文王元年十一月") should be (date(-250, 12, 19))

      // Check historical dates from zh.wikipedia.org.
      // 秦王政即位
      toDate("秦莊襄王三年五月丙午") should be (date(-246, 7, 6))
      // 秦始皇崩 (http://blog.sina.com.cn/s/blog_50823c400100iqqw.html)
      toDate("秦始皇三十七年八月丙寅") should be (date(-209, 9, 10))
      // 漢高祖崩
      toDate("漢高祖十二年四月甲辰") should be (date(-194, 6, 1))
      // 漢惠帝崩
      toDate("漢惠帝七年八月戊寅") should be (date(-187, 9, 26))
      // 漢高后崩
      toDate("漢高后八年七月辛巳") should be (date(-179, 8, 18))
      // 漢文帝崩
      toDate("漢文帝後七年六月己亥") should be (date(-156, 7, 6))
      // 漢景帝崩
      toDate("漢景帝後三年正月甲子") should be (date(-140, 3, 9))
      // 漢昭帝即位
      toDate("漢武帝後元二年二月戊辰") should be (date(-86, 3, 30))
      // 漢昭帝崩
      toDate("漢昭帝元平元年四月癸未") should be (date(-73, 6, 5))
      // 昌邑王即位
      toDate("漢昭帝元平元年六月丙寅") should be (date(-73, 7, 18))
      // 昌邑王被废黜
      toDate("漢昭帝元平元年六月癸巳") should be (date(-73, 8, 14))
      // 漢宣帝即位
      toDate("漢昭帝元平元年七月庚申") should be (date(-73, 9, 10))
      // 漢宣帝崩
      toDate("漢宣帝黃龍元年十二月甲戌") should be (date(-47, 1, 10))
      // 漢平帝即位
      toDate("漢哀帝元壽二年九月辛酉") should be (date(0, 10, 17))

      // Test for calendar system change.
      toDate("漢武帝太初元年十月朔") should be (date(-104, 11, 26))
      toDate("漢武帝太初元年十月初二") should be (date(-104, 11, 27))

      // Check based on book tables.
      toDate("漢平帝元始元年") should be (date(1, 2, 12))
      toDate("漢平帝元始二年") should be (date(2, 2, 2))
      toDate("漢平帝元始三年") should be (date(3, 2, 21))
      toDate("漢平帝元始元年一月朔") should be (date(1, 2, 12))
      toDate("漢平帝元始元年正月朔") should be (date(1, 2, 12))
      toDate("漢平帝元始元年正月初二") should be (date(1, 2, 13))
      toDate("漢平帝元始元年一月十一") should be (date(1, 2, 22))
      toDate("漢平帝元始元年一月晦") should be (date(1, 3, 13))
      toDate("漢平帝元始元年二月朔") should be (date(1, 3, 14))
      toDate("漢平帝元始元年二月十一") should be (date(1, 3, 24))
      toDate("漢平帝元始元年二月己丑") should be (date(1, 3, 14))
      toDate("漢平帝元始元年二月己亥") should be (date(1, 3, 24))
      toDate("漢平帝元始元年二月己酉") should be (date(1, 4, 3))
      toDate("漢平帝元始元年春二月己酉") should be (date(1, 4, 3))
      toDate("漢平帝元始元年三月廿一") should be (date(1, 5, 2))
      toDate("漢平帝元始四年二月十一") should be (date(4, 3, 20))
      toDate("魏明帝景初元年四月初一") should be (date(237, 4, 13))
      toDate("魏明帝景初三年後十二月初一") should be (date(240, 1, 12))
      toDate("晉武帝咸寧元年") should be (date(275, 2, 13))

      // Additional tests based on bug fixes.
      toDate("魏陳留王咸熙元年五月") should be (date(264, 6, 12))

      // Test for 唐武后
      toDate("唐武后載初元年正月") should be (date(689, 12, 18))
      toDate("唐武后天授三年臘月") should be (date(691, 12, 25))
      toDate("唐武后證聖元年閏二月") should be (date(695, 3, 21))
      toDate("唐武后久視元年十一月") should be (date(700, 12, 15))
      toDate("唐武后大足元年正月") should be (date(701, 2, 13))

      // Test for 載
      toDate("唐玄宗天寶二年十二月三十") should be (date(744, 1, 19))
      toDate("唐玄宗天寶三載正月") should be (date(744, 1, 20))
      toDate("唐肅宗至德三載正月晦") should be (date(758, 3, 13))
      toDate("唐肅宗乾元元年二月") should be (date(758, 3, 14))

      // Test for 唐肅宗
      toDate("唐肅宗元年建寅月初一") should be (date(762, 1, 30))

      toDate("公元一九五一年二月初一") should be (date(1951, 3, 8))
    }

    it("Check fromDate().") {
      fromDate(date(1, 2, 12)) should === (List("漢平帝元始元年正月初一"))
      fromDate(date(1, 2, 22)) should === (List("漢平帝元始元年正月十一"))
      fromDate(date(237, 4, 13)) should === (List("魏明帝景初元年四月初一", "蜀後主建興十五年三月初一", "吳大帝嘉禾六年三月初一"))

      // Check DynastyOrder.
      fromDate(date(543, 5, 6)) should === (List("梁武帝大同九年三月十七", "西魏文帝大統九年三月十七", "東魏孝靜帝武定元年三月十七"))

      // Test for 唐武后
      fromDate(date(689, 12, 18)) should === (List("唐武后載初元年正月初一"))

      // Test for 載
      fromDate(date(744, 1, 19)) should === (List("唐玄宗天寶二年十二月三十"))
      fromDate(date(744, 1, 20)) should === (List("唐玄宗天寶三載正月初一"))
      fromDate(date(758, 3, 13)) should === (List("唐肅宗至德三載正月廿九"))
      fromDate(date(758, 3, 14)) should === (List("唐肅宗乾元元年二月初一"))
    }

    it("Check monthLength().") {
      monthLength("漢武帝太初元年十月") should be (29)
      monthLength("漢武帝太初元年九月") should be (29)
      monthLength("漢武帝太初二年九月") should be (30)
      monthLength("漢平帝元始元年正月") should be (30)
      monthLength("漢平帝元始元年二月") should be (29)
      monthLength("漢平帝元始元年十二月") should be (30)
      monthLength("魏高貴鄉公甘露元年十二月") should be (29)
      monthLength("吳會稽王太平元年十二月") should be (30)
    }

    it("Check plusDays().") {
      // Within same month.
      parseDate("漢平帝元始元年二月十一").plusDays(10) should be (parseDate("漢平帝元始元年二月廿一"))
      parseDate("漢平帝元始元年二月己丑").plusDays(11) should be (parseDate("漢平帝元始元年二月庚子"))
      parseDate("漢平帝元始元年二月廿一").plusDays(-10) should be (parseDate("漢平帝元始元年二月十一"))
      parseDate("漢平帝元始元年二月庚子").plusDays(-11) should be (parseDate("漢平帝元始元年二月己丑"))
      parseDate("漢平帝元始元年二月晦").plusDays(-10) should be (parseDate("漢平帝元始元年二月十九"))

      // Different months, positive input.
      parseDate("漢平帝元始元年").plusDays(30) should be (parseDate("漢平帝元始元年二月初一"))
      parseDate("漢平帝元始元年二月初一").plusDays(29) should be (parseDate("漢平帝元始元年三月初一"))
      parseDate("漢平帝元始元年二月初一").plusDays(60) should be (parseDate("漢平帝元始元年四月初二"))
      parseDate("漢平帝元始元年十二月初一").plusDays(31) should be (parseDate("漢平帝元始二年一月初二"))
      parseDate("漢平帝元始元年二月己丑").plusDays(30) should be (parseDate("漢平帝元始元年三月初二"))

      // Diferent months, negative input.
      parseDate("漢平帝元始元年二月初一").plusDays(-30) should be (parseDate("漢平帝元始元年一月初一"))
      parseDate("漢平帝元始元年三月初一").plusDays(-29) should be (parseDate("漢平帝元始元年二月初一"))
      parseDate("漢平帝元始元年四月初二").plusDays(-60) should be (parseDate("漢平帝元始元年二月初一"))
      parseDate("漢平帝元始二年一月初二").plusDays(-31) should be (parseDate("漢平帝元始元年十二月初一"))
      parseDate("漢平帝元始元年三月初二").plusDays(-30) should be (parseDate("漢平帝元始元年二月初一"))
      parseDate("漢平帝元始元年").plusDays(-1) should be (parseDate("漢哀帝元壽二年十二月廿九"))
    }

    it("Check containingSegments().") {
      val eraSegment1 = EraSegment("漢哀帝元壽", parseDate("漢哀帝元壽元年正月初一"),
        date(1, 2, 11), "漢哀帝建平", "漢平帝元始")
      containingSegments(date(1, 2, 3)) should be (List(eraSegment1))
      containingSegment(parseDate("漢哀帝元壽元年正月初一")) should be (Some(eraSegment1))

      val eraSegment2 = EraSegment("魏明帝青龍", parseDate("魏明帝青龍元年二月初一"),
        date(237, 4, 12), "魏明帝太和", "魏明帝景初")
      val eraSegment3 = EraSegment("蜀後主建興", parseDate("蜀後主建興元年五月初一"),
        date(238, 2, 1), "蜀昭烈帝章武", "蜀後主延熙")
      val eraSegment4 = EraSegment("吳大帝嘉禾", parseDate("吳大帝嘉禾元年正月初一"),
        date(238, 8, 27), "吳大帝黃龍", "吳大帝赤烏")
      containingSegments(date(234, 5, 6)) should be (List(eraSegment2, eraSegment3, eraSegment4))
    }

    it("Check firstDayNextMonth().") {
      parseDate("漢惠帝七年九月初八").firstDayNextMonth(false) should be (parseDate("漢高后元年十月初一"))
      parseDate("漢武帝元朔五年後九月十二").firstDayNextMonth(false) should be (parseDate("漢武帝元朔六年十月初一"))
      parseDate("漢武帝元朔六年九月十四").firstDayNextMonth(false) should be (parseDate("漢武帝元狩元年十月初一"))
      parseDate("漢武帝元封六年後九月十七").firstDayNextMonth(false) should be (parseDate("漢武帝太初元年十月初一"))
      parseDate("漢平帝元始元年二月己丑").firstDayNextMonth(false) should be (parseDate("漢平帝元始元年三月初一"))
      parseDate("蜀昭烈帝章武三年四月初二").firstDayNextMonth(false) should be (parseDate("蜀後主建興元年五月初一"))
      parseDate("蜀後主炎興元年十一月十一").firstDayNextMonth(false) should be (parseDate("魏陳留王景元四年十二月初一"))
      parseDate("隋文帝開皇二十年十二月朔").firstDayNextMonth(false) should be (parseDate("隋文帝仁壽元年一月初一"))

      // Test for 載.
      parseDate("唐玄宗天寶二年十二月三十").firstDayNextMonth(false).year should be ("三載")
      parseDate("唐肅宗至德三載正月初一").firstDayNextMonth(false).year should be ("一年")
    }

    it("Check lastDayPrevMonth().") {
      parseDate("漢平帝元始元年三月初一").lastDayPrevMonth(false) should be (parseDate("漢平帝元始元年二月廿九"))
      parseDate("蜀昭烈帝章武元年四月").lastDayPrevMonth(false) should be (parseDate("魏文帝黃初二年三月三十"))
      parseDate("蜀後主建興元年五月初一").lastDayPrevMonth(false) should be (parseDate("蜀昭烈帝章武三年四月廿九"))
    }

    it("Check sameDayNextMonth().") {
      parseDate("漢惠帝七年九月初八").sameDayNextMonth() should be (parseDate("漢高后元年十月初八"))
      parseDate("漢惠帝七年九月三十").sameDayNextMonth() should be (parseDate("漢高后元年十月晦"))

      checkSameDayNextMonth()
    }

    it("Check sameDayPrevMonth().") {
      parseDate("漢惠帝七年九月初八").sameDayPrevMonth() should be (parseDate("漢惠帝七年八月初八"))
      parseDate("漢惠帝七年九月三十").sameDayPrevMonth() should be (parseDate("漢惠帝七年八月晦"))

      checkSameDayPrevMonth()
    }

    it("Check sameDayNextYear().") {
      parseDate("漢惠帝七年九月初八").sameDayNextYear() should be (parseDate("漢高后元年九月初八"))
      parseDate("漢惠帝七年九月三十").sameDayNextYear() should be (parseDate("漢高后元年九月晦"))
      parseDate("漢武帝元封六年九月十四").sameDayNextYear() should be (parseDate("漢武帝太初元年九月十四"))
      parseDate("漢武帝元封六年後九月十七").sameDayNextYear() should be (parseDate("漢武帝太初元年九月十七"))
      parseDate("漢武帝太初元年九月十七").sameDayNextYear() should be (parseDate("漢武帝太初二年九月十七"))
      parseDate("魏明帝青龍四年三月十七").sameDayNextYear() should be (parseDate("魏明帝景初元年四月十七"))
      parseDate("魏明帝青龍五年一月初八").sameDayNextYear() should be (parseDate("魏明帝景初二年一月初八"))
      parseDate("魏明帝青龍五年二月初七").sameDayNextYear() should be (parseDate("魏明帝景初二年二月初七"))

      checkSameDayNextYear()
    }

    it("Check sameDayPrevYear().") {
      parseDate("漢高后元年九月初八").sameDayPrevYear() should be (parseDate("漢惠帝七年九月初八"))
      parseDate("漢高后元年九月晦").sameDayPrevYear() should be (parseDate("漢惠帝七年九月晦"))
      parseDate("漢武帝太初元年九月十四").sameDayPrevYear() should be (parseDate("漢武帝元封六年九月十四"))
      parseDate("漢武帝太初元年後十月十七").sameDayPrevYear() should be (parseDate("漢武帝太初元年十月十七"))
      parseDate("漢武帝太初二年九月十七").sameDayPrevYear() should be (parseDate("漢武帝太初元年九月十七"))
      parseDate("魏明帝景初元年四月十七").sameDayPrevYear() should be (parseDate("魏明帝青龍四年四月十七"))
      parseDate("魏明帝景初二年一月初八").sameDayPrevYear() should be (parseDate("魏明帝青龍五年一月初八"))
      parseDate("魏明帝景初二年二月初七").sameDayPrevYear() should be (parseDate("魏明帝青龍五年二月初七"))
      parseDate("魏明帝景初二年三月初七").sameDayPrevYear() should be (parseDate("魏明帝青龍五年二月初七"))

      checkSameDayPrevYear()
    }

    it("Check yearSexagenary().") {
      // BCEYears
      parseDate("秦昭襄王五十四年").yearSexagenary() should be ("戊申")
      parseDate("漢武帝征和元年").yearSexagenary() should be ("己丑")

      // CEYears
      parseDate("漢平帝元始二年二月晦").yearSexagenary() should be ("壬戌")
      parseDate("晉穆帝永和九年三月初三").yearSexagenary() should be ("癸丑")

      // ShuYears
      parseDate("蜀後主景耀三年").yearSexagenary() should be ("庚辰")

      // WuYears
      parseDate("吳會稽王太平三年").yearSexagenary() should be ("戊寅")
    }

    it("Check sexagenaries().") {
      sexagenaries("甲子", 3).mkString(" ") should be ("甲子 乙丑 丙寅")
      // Test wrap around.
      sexagenaries("辛酉", 5).mkString(" ") should be ("辛酉 壬戌 癸亥 甲子 乙丑")
    }

    it("Check nextCharacter().") {
      nextCharacter("公元前842年") should === (Array("1"))
      nextCharacter("公元前842年1") should === (Array("2"))
      nextCharacter("公元前842年12") should === (Array("月"))
      nextCharacter("公元前842年12月1") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      nextCharacter("公元前5") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "年"))
      nextCharacter("公元前5年") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      nextCharacter("公元前5年1") should === (Array("1", "2", "0", "月"))
      nextCharacter("公元前5年2") should === (Array("月"))
      nextCharacter("公元前5年10") should === (Array("月"))
      nextCharacter("公元前5年2月") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      nextCharacter("公元前5年2月1") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "日"))
      nextCharacter("公元前5年2月12") should === (Array("日"))
      nextCharacter("公元前5年2月2") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "日"))
      nextCharacter("353年") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      nextCharacter("353年1") should === (Array("1", "2", "0", "月"))
      nextCharacter("353年2") should === (Array("月"))
      nextCharacter("353年10") should === (Array("月"))
      nextCharacter("353年2月") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "9"))
      nextCharacter("353年2月1") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "9", "0", "日"))
      nextCharacter("353年2月12") should === (Array("日"))
      nextCharacter("353年2月2") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "0", "日"))
      nextCharacter("353年2月3") should === (Array("日"))
      nextCharacter("353年3月3") should === (Array("1", "0", "日"))
      nextCharacter("353年4月3") should === (Array("0", "日"))
      nextCharacter("1582年10月") should === (Array("1", "2", "3", "4"))
      nextCharacter("1582年10月1") should === (Array("5", "6", "7", "8", "9", "日"))
      nextCharacter("2101年") should === (Array("1"))
      nextCharacter("2101年1") should === (Array("月"))
      nextCharacter("2101年1月2") should === (Array("1", "2", "3", "4", "5", "6", "7", "8", "0", "日"))

      nextCharacter("秦王政") should === (Array("元", "二", "三", "四", "五", "六", "七", "八", "九", "十"))
      nextCharacter("秦王政元") should === (Array("年"))
      nextCharacter("秦王政十") should === (Array("一", "二", "三", "四", "五", "六", "七", "八", "九", "年"))
      nextCharacter("秦王政二") should === (Array("十", "年"))
      nextCharacter("秦王政二十") should === (Array("一", "二", "三", "四", "五", "年"))
      nextCharacter("秦始皇") should === (Array("二", "三"))
      nextCharacter("秦始皇二十") should === (Array("六", "七", "八", "九"))
      nextCharacter("秦始皇三十") should === (Array("一", "二", "三", "四", "五", "六", "七", "年"))
      nextCharacter("秦始皇三十六年") should === (Array("正", "二", "三", "四", "五", "六", "七", "八", "九", "十"))
      nextCharacter("秦始皇三十六年十") should === (Array("一", "二", "月"))
      nextCharacter("秦始皇三十六年十二") should === (Array("月"))
      nextCharacter("秦始皇三十六年十二月") should === (Array("甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸", "初", "十", "二", "廿", "三", "朔", "晦"))
      nextCharacter("秦始皇三十六年十二月初") should === (Array("一", "二", "三", "四", "五", "六", "七", "八", "九", "十"))
      nextCharacter("秦始皇三十六年十二月朔") should === (Array(""))
      nextCharacter("漢獻帝中平六年") should === (Array("十", "閏"))
      nextCharacter("漢獻帝中平六年十") should === (Array("二"))
      nextCharacter("漢獻帝中平六年十二月") should === (Array("甲", "乙", "丙", "丁", "戊", "己", "庚", "辛", "壬", "癸", "初", "十", "二", "廿", "朔", "晦"))

      // Check the order of results.
      nextCharacter("唐").startsWith(Array("高", "太")) should be (true)

      // Check the handling of 正月 and 臘月.
      nextCharacter("唐武后載初元年") should === (Array("一", "二", "三", "四", "五", "六", "七", "八", "正", "臘"))
      nextCharacter("唐武后長安二年") should === (Array("正", "二", "三", "四", "五", "六", "七", "八", "九", "十"))

      // Check the handling of 載.
      nextCharacter("唐肅宗至德三") should === (Array("載"))
      nextCharacter("唐肅宗至德三載") should === (Array("正"))
      nextCharacter("唐武后載") should === (Array("初"))
      nextCharacter("唐武后延載") should === (Array("元"))

      // Check the handling of 唐肅宗.
      nextCharacter("唐肅宗") should === (Array("至", "乾", "上", "寶", "元"))
      nextCharacter("唐肅宗寶") should === (Array("應"))
      nextCharacter("唐肅宗寶應") should === (Array("元", "二"))

      // Check for 民國
      nextCharacter("民") should === (Array("國"))
      nextCharacter("民國") should === (Array("元", "二", "三", "四", "五", "六", "七", "八", "九", "十"))

      // Check for 公元
      nextCharacter("公元一") should === (Array("九"))
      nextCharacter("公元一九") should === (Array("四", "五", "六", "七", "八", "九"))
      nextCharacter("公元一九四") should === (Array("九"))
      //nextCharacter("公元二") should === (Array("〇", "一"))
      //nextCharacter("公元二〇") should === (Array("〇", "一", "二", "三", "四", "五", "六", "七", "八", "九"))
      //nextCharacter("公元二一") should === (Array("〇"))

      for (era <- eraNames()) {
        for (i <- 1 to era.length) {
          val next = nextCharacter(era.substring(0, i))
          next.length should be >= 1
          next(0) should have length 1
        }
      }
    }

    it("Check simplified2Traditional2().") {
      simplified2Traditional2("遼道宗咸") should be ("遼道宗咸")
      simplified2Traditional2("光绪") should be ("光緒")
      simplified2Traditional2("光緒") should be ("光緒")
    }

    it("Check data sanity.") {
      sanityCheck should be (true)
    }

    it("Check Simplified/Traditional Chinese conversion of era names.") {
      for (eraName <- eraNames()) {
        simplified2Traditional(traditional2Simplified(eraName)) should be (eraName)
      }
    }

    it("Check conversion for every day.") {
      checkEveryDay() should be (true)
    }
  }
}
