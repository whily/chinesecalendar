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
    }

    it("Check parseDate().") {
      parseDate("漢武帝元朔六年") should be (parseDate("漢武帝元朔六年十月初一"))                  
    }

    it("Check toDate().") {
      // Check historical dates from zh.wikipedia.org.
      // TODO: 秦孝文王即位
      // TODO: toDate("秦孝文王元年十月己亥") should be (date(-250, 11, 12))
      toDate("秦孝文王元年十一月") should be (date(-250, 12, 19))
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
    }

    it("Check fromDate().") {
      fromDate(date(1, 2, 12)) should === (List("漢平帝元始元年正月初一"))
      fromDate(date(1, 2, 22)) should === (List("漢平帝元始元年正月十一"))
      fromDate(date(237, 4, 13)) should === (List("魏明帝景初元年四月初一", "蜀後主建興十五年三月初一", "吳大帝嘉禾六年三月初一"))
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

    it("Check firstDayNextMonth().") {
      parseDate("漢惠帝七年九月初八").firstDayNextMonth(false) should be (parseDate("漢高后元年十月初一"))
      parseDate("漢武帝元朔五年後九月十二").firstDayNextMonth(false) should be (parseDate("漢武帝元朔六年十月初一"))            
      parseDate("漢武帝元朔六年九月十四").firstDayNextMonth(false) should be (parseDate("漢武帝元狩元年十月初一"))
      parseDate("漢武帝元封六年後九月十七").firstDayNextMonth(false) should be (parseDate("漢武帝太初元年十月初一"))                  
      parseDate("漢平帝元始元年二月己丑").firstDayNextMonth(false) should be (parseDate("漢平帝元始元年三月初一"))
      parseDate("蜀昭烈帝章武三年四月初二").firstDayNextMonth(false) should be (parseDate("蜀後主建興元年五月初一"))      
      parseDate("蜀後主炎興元年十一月十一").firstDayNextMonth(false) should be (parseDate("魏陳留王景元四年十二月初一"))            
    }

    it("Check lastDayPrevMonth().") {
      parseDate("漢平帝元始元年三月初一").lastDayPrevMonth(false) should be (parseDate("漢平帝元始元年二月廿九"))
      parseDate("蜀昭烈帝章武元年四月").lastDayPrevMonth(false) should be (parseDate("魏文帝黃初二年三月三十"))                  
      parseDate("蜀後主建興元年五月初一").lastDayPrevMonth(false) should be (parseDate("蜀昭烈帝章武三年四月廿九"))      
    }

    it("Check sameDayNextMonth().") {
      parseDate("漢惠帝七年九月初八").sameDayNextMonth() should be (parseDate("漢高后元年十月初八"))
      parseDate("漢惠帝七年九月三十").sameDayNextMonth() should be (parseDate("漢高后元年十月晦"))

      // TODO: cover the whole range.
      var d = parseDate("秦孝文王元年")
      while (toDate(d, true) < date(200, 1, 1)) {
        d = d.sameDayNextMonth()
      }
    }

    it("Check sameDayPrevMonth().") {
      parseDate("漢惠帝七年九月初八").sameDayPrevMonth() should be (parseDate("漢惠帝七年八月初八"))
      parseDate("漢惠帝七年九月三十").sameDayPrevMonth() should be (parseDate("漢惠帝七年八月晦"))

      // TODO: cover the whole range.
      var d = parseDate("齊東昏侯永元元年")
      while (toDate(d, true) > date(100, 1, 1)) {
        d = d.sameDayPrevMonth()
      }      
    }    

    it("Check yearSexagenary().") {
      // BCEYears
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

    it("Check data sanity.") {
      sanityCheck should be (true)
    }

    it("Check Simplified/Traditional Chinese conversion of era names.") {
      for (eraName <- eraNames()) {
        simplified2Traditional(traditional2Simplified(eraName)) should be (eraName)
      }
    }

    it("Check conversion for every day.") {
      checkEveryDay()  should be (true)
    }
  }
}
