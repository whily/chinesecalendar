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
      simplified2Traditional("补偿") shouldBe "補償"
      traditional2Simplified("補償") shouldBe "补偿"
      simplified2Traditional("阑干") shouldBe "闌乾"
      traditional2Simplified("闌乾") shouldBe "阑干"            

      // Check for exceptions.
      simplified2Traditional("乾隆") shouldBe "乾隆"
      traditional2Simplified("乾隆") shouldBe "乾隆"      
      simplified2Traditional("宫商角徵羽") shouldBe "宫商角徵羽"
      traditional2Simplified("宫商角徵羽") shouldBe "宫商角徵羽"                 
    }
  }
}
