/**
 * Test cases for Chinese.scala.
 * 
 * @author  Yujian Zhang <yujian{dot}zhang[at]gmail(dot)com>
 *
 * License: 
 *   GNU General Public License v2
 *   http://www.gnu.org/licenses/gpl-2.0.html
 * Copyright (C) 2015 Yujian Zhang
 */

import net.whily.chinesecalendar.Chinese._
import org.scalatest._
 
class ChineseTest extends FunSpec with Matchers {
  describe("Test Simplified/Traditional Chinese conversion") {
    it("Check conversion") {
      Simplified2Traditional("补偿") shouldBe "補償"
      Traditional2Simplified("補償") shouldBe "补偿"
    }
  }
}
