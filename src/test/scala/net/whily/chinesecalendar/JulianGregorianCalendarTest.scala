/**
 * Test cases for JulianGregorianCalendar.scala.
 * 
 * @author  Yujian Zhang <yujian{dot}zhang[at]gmail(dot)com>
 *
 * License: 
 *   GNU General Public License v2
 *   http://www.gnu.org/licenses/gpl-2.0.html
 * Copyright (C) 2015 Yujian Zhang
 */

import net.whily.chinesecalendar.JulianGregorianCalendar._
import net.whily.chinesecalendar.ChineseCalendar._
import org.scalatest._
 
class JulianGregorianCalendarTest extends FunSpec with Matchers {
  describe("Test Date calculation") {
    it("Check isLeapyear()") {
      date(-41, 3, 9) should not be 'isLeapYear
      date(-40, 4, 6) shouldBe 'isLeapYear      
      date(4, 2, 3) shouldBe 'isLeapYear
      date(8, 7, 6) shouldBe 'isLeapYear
      date(11, 11, 11) should not be 'isLeapYear
      date(1600, 1, 2) shouldBe 'LeapYear
      date(1700, 2, 3) should not be 'LeapYear
    }

    it("Check fromString()") {
      fromString("1年2月3日") should be (date(1, 2, 3))
      fromString("公元前1年2月3日") should be (date(0, 2, 3))      
    }

    it("Check fromStringFrag()") {
      fromStringFrag("1年") should be (date(1, 1, 1))
      fromStringFrag("公元前1年2月") should be (date(0, 1, 1))          
    }

    it("Check plusDays()") {
      // Check when daysToAdd = 0
      date(1582, 10, 1).plusDays(0) should be (date(1582, 10, 1))

      // Check when daysToAdd > 0
      date(-41, 3, 9).plusDays(366) should be (date(-40, 3, 9))
      date(17, 2, 3).plusDays(365) should be (date(18, 2, 3))
      date(17, 2, 3).plusDays(366) should be (date(18, 2, 4))
      date(1999, 12, 31).plusDays(365) should be (date(2000, 12, 30))
      date(1999, 12, 31).plusDays(366) should be (date(2000, 12, 31))
      // Year 4 CE is a leap year.
      date(4, 2, 28).plusDays(1) should be (date(4, 2, 29))
      // Check Julian/Gregorian calendar cut over.
      date(1582, 10, 2).plusDays(2) should be (date(1582, 10, 4))
      date(1582, 10, 3).plusDays(2) should be (date(1582, 10, 15))
      date(1582, 10, 4).plusDays(3) should be (date(1582, 10, 17))

      // Check when daysToAdd < 0
      date(-40, 3, 9).plusDays(-366) should be (date(-41, 3, 9))
      date(18, 2, 3).plusDays(-365) should be (date(17, 2, 3))
      date(18, 2, 4).plusDays(-366) should be (date(17, 2, 3))
      date(2000, 12, 30).plusDays(-365) should be (date(1999, 12, 31))
      date(2000, 12, 31).plusDays(-366) should be (date(1999, 12, 31))
      // Year 4 CE is not a leap year.
      date(4, 3, 1).plusDays(-1) should be (date(4, 2, 29))
      // Check Julian/Gregorian calendar cut over.
      date(1582, 10, 4).plusDays(-2) should be (date(1582, 10, 2))
      date(1582, 10, 15).plusDays(-2) should be (date(1582, 10, 3))
      date(1582, 10, 17).plusDays(-3) should be (date(1582, 10, 4))      
    }

    it("Check ordering.") {
      date(1, 9, 10) should be < date(2, 3, 4)
      date(1, 2, 12) should be < date(1, 3, 4)
      date(1, 2, 3) should be < date(1, 2, 9)
    }

    it("Check toJdn().") {
      date(-4712, 1, 1).toJdn() should be (0)
      date(1582, 10, 4).toJdn() should be (date(1582, 10, 15).toJdn() - 1)
      date(2000, 1, 1).toJdn() should be (2451545)
    }

    it("Check binary -.") {
      date(2000, 12, 31) - date(1999, 12, 31) should be (366)
      date(1582, 10, 4) - date(1582, 10, 15) should be (-1)
    }
  }
}
