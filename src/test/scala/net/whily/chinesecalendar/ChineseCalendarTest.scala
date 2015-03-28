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
import org.scalatest._
 
class ChineseCalendarTest extends FunSpec with Matchers {
  describe("Test Chinese Calendar calculation") {
    it("Check months()") {
      // It is interesting that I should use "shoulbe be ===" to test two arrays. 
      // Using "shoulbe be" always returns true, while using "shoulbe equal" cannot compile.
      months("己未 己丑 戊午 戊子 丁巳 丁亥 丙辰 丙戌 丙辰 乙酉 乙卯 甲申") should be === (Array(
        Month("一月", "己未"), Month("二月", "己丑"), Month("三月", "戊午"),
        Month("四月", "戊子"), Month("五月", "丁巳"), Month("六月", "丁亥"),
        Month("七月", "丙辰"), Month("八月", "丙戌"), Month("九月", "丙辰"),
        Month("十月", "乙酉"), Month("十一月", "乙卯"), Month("十二月", "甲申")))
      months("甲寅 癸未 癸丑 壬午 壬子 辛巳 辛亥 庚辰 閏 庚戌 己卯 己酉 戊寅 戊申") should be === (Array(
        Month("一月", "甲寅"), Month("二月", "癸未"), Month("三月", "癸丑"),
        Month("四月", "壬午"), Month("五月", "壬子"), Month("六月", "辛巳"),
        Month("七月", "辛亥"), Month("八月", "庚辰"), Month("閏八月", "庚戌"),
        Month("九月", "己卯"), Month("十月", "己酉"), Month("十一月", "戊寅"),
        Month("十二月", "戊申")))
      months("己卯 閏 戊申 戊寅 戊申 丁丑 丁未 丙子 丙午 乙亥 乙巳 甲戌 甲辰 癸酉") should be === (Array(
        Month("一月", "己卯"), Month("閏一月", "戊申"), Month("二月", "戊寅"),
        Month("三月", "戊申"), Month("四月", "丁丑"), Month("五月", "丁未"),
        Month("六月", "丙子"), Month("七月", "丙午"), Month("八月", "乙亥"),
        Month("九月", "乙巳"), Month("十月", "甲戌"), Month("十一月", "甲辰"),
        Month("十二月", "癸酉")))
      months("己亥 己巳 進 戊戌 戊辰 丁酉 丙寅 丙申 乙丑 乙未 甲子 甲午") should be === (Array(
        Month("一月", "己亥"), Month("二月", "己巳"), Month("四月", "戊戌"),
        Month("五月", "戊辰"), Month("六月", "丁酉"), Month("七月", "丙寅"),
        Month("八月", "丙申"), Month("九月", "乙丑"), Month("十月", "乙未"),
        Month("十一月", "甲子"), Month("十二月", "甲午")))
      months("丁亥 丁巳 丙戌 丙辰 乙酉 乙卯 甲申 甲寅 癸未 癸丑 壬午 壬子 後 壬午") should be === (Array(
        Month("一月", "丁亥"), Month("二月", "丁巳"), Month("三月", "丙戌"),
        Month("四月", "丙辰"), Month("五月", "乙酉"), Month("六月", "乙卯"),
        Month("七月", "甲申"), Month("八月", "甲寅"), Month("九月", "癸未"),
        Month("十月", "癸丑"), Month("十一月", "壬午"), Month("十二月", "壬子"),
        Month("後十二月", "壬午")))
    }

    it("Check date.") {
      // 漢平帝即位
      toDate("漢哀帝元壽二年九月辛酉") should be (date(0, 10, 17))

      toDate("漢平帝元始元年") should be (date(1, 2, 12))
      toDate("元始元年") should be (date(1, 2, 12))
      toDate("元始二年") should be (date(2, 2, 2))
      toDate("元始三年") should be (date(3, 2, 21))
      toDate("元始元年一月朔") should be (date(1, 2, 12))
      toDate("元始元年正月朔") should be (date(1, 2, 12))
      toDate("元始元年正月初二") should be (date(1, 2, 13))
      toDate("元始元年一月十一") should be (date(1, 2, 22))            
      toDate("元始元年二月朔") should be (date(1, 3, 14))
      toDate("元始元年二月十一") should be (date(1, 3, 24))      
      toDate("元始元年二月己丑") should be (date(1, 3, 14))
      toDate("元始元年二月己亥") should be (date(1, 3, 24))
      toDate("元始元年二月己酉") should be (date(1, 4, 3))
      toDate("元始元年春二月己酉") should be (date(1, 4, 3))
      toDate("元始元年三月廿一") should be (date(1, 5, 2))
      toDate("元始四年二月十一") should be (date(4, 3, 20))

      toDate("景初元年四月初一") should be (date(237, 4, 13))
      toDate("景初三年後十二月初一") should be (date(240, 1, 12))      
    }

    it("Check monthLength().") {
      monthLength("元始元年正月") should be (30)
      monthLength("元始元年二月") should be (29)
      monthLength("元始元年十二月") should be (30)
      monthLength("魏高貴鄉公甘露元年十二月") should be (29)
      monthLength("吳會稽王太平元年十二月") should be (30)            
    }

    it("Check sexagenaries().") {
      sexagenaries("甲子", 3).mkString(" ") should be ("甲子 乙丑 丙寅")
      // Test wrap around.
      sexagenaries("辛酉", 5).mkString(" ") should be ("辛酉 壬戌 癸亥 甲子 乙丑")      
    }

    it("Check data sanity.") {
      sanityCheck should be (true)
    }
  }
}
