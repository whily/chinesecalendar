/**
 * Simplified/Traditional Chinese conversion.
 *
 * @author  Yujian Zhang <yujian{dot}zhang[at]gmail(dot)com>
 *
 * License: 
 *   GNU General Public License v2
 *   http://www.gnu.org/licenses/gpl-2.0.html
 * Copyright (C) 2015 Yujian Zhang
 */

package net.whily.chinesecalendar

/** Convert between Simplified and Traditional Chinese.
  * See http://www.kjqk.gd.cn/HTML/zlzx/12785517318915120561875011743515.html 
  */
object Chinese {
  /** Convert Simplified Chinese to Traditional Chinese. */
  def Simplifed2Traditional(s: String) = s

  /** Convert Traditional Chinese to Simplified Chinese. */
  def Traditional2Simplified(s: String) = s

  /* 简化字总表（1986年新版）. 
   * 
   * For single character only.  
   * 
   * For each element, the first character is Simplified Chinese, and
   * remaining characters are corresponding Traditional Chinese.
   */
  private val CharacterTable = List(
    // 第一表: 不作简化偏旁用的简化字
    "碍礙", "肮骯", "袄襖", "坝壩", "板闆", "办辦", "帮幫", "宝寶", "报報", "币幣",
    "毙斃", "标標", "表錶", "别彆", "卜蔔", "补補", "才纔", "蚕蠶", "灿燦", "层層",
    "搀攙", "谗讒", "馋饞", "缠纏", "忏懺", "偿償", "厂廠", "彻徹", "尘塵", "衬襯",
    "称稱", "惩懲", "迟遲", "冲衝", "丑醜", "出齣", "础礎", "处處", "触觸", "辞辭",
    "聪聰", "丛叢"  
  )
}
