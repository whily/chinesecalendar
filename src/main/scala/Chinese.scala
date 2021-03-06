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

import scala.collection.mutable

/** Convert between Simplified and Traditional Chinese.
  * See http://www.kjqk.gd.cn/HTML/zlzx/12785517318915120561875011743515.html
  */
object Chinese {
  /** Convert Simplified Chinese to Traditional Chinese. */
  def simplified2Traditional(s: String) =
    chineseConvCore(s, simplified2TraditionalMaps)

  /** Convert Traditional Chinese to Simplified Chinese. */
  def traditional2Simplified(s: String) =
    chineseConvCore(s, traditional2SimplifiedMaps)

  def chineseConvCore(s: String, maps: (mutable.HashMap[Char, Char], mutable.HashMap[String, String])) = {
    val (characterMap, wordMap) = maps
    val result = s.toArray
    var index = 0
    while (index < s.length) {
      var wordMatch = false
      var wordLength = maxWordLength
      if (s.length - index < wordLength) {
        wordLength = s.length - index
      }
      while ((wordLength >= 2) && !wordMatch) {
        val w = s.substring(index, index + wordLength)
        if (wordMap.contains(w)) {
          val v = wordMap(w)
          for (i <- Range(0, wordLength)) {
            result(index + i) = v(i)
          }
          wordMatch = true
          index += wordLength
        }
        wordLength -= 1
      }
      if (!wordMatch) {
        val c = s(index)
        if (characterMap.contains(c)) {
          result(index) = characterMap(c)
        }
        index += 1
      }

    }
    result.mkString("")
  }

  /* 简化字总表（1986年新版）.
   *
   * For single character only.
   *
   * For each element, the first character is Simplified Chinese, and
   * remaining characters are corresponding Traditional Chinese.
   *
   * The first Traditional Chinese character will be used when
   * converting from Simplified Chinese.
   *
   * Note: in Emacs, some characters below (e.g. ) might not look
   * correct. They are actually fine (e.g. viewed in web browser).
   */
  private val CharacterTable = List(
    // 第一表: 不作简化偏旁用的简化字
    "碍礙", "肮骯", "袄襖", "坝壩", "板闆", "办辦", "帮幫", "宝寶", "报報", "币幣",
    "毙斃", "标標", "表錶", "别彆", "卜蔔", "补補", "才纔", "蚕蠶", "灿燦", "层層",
    "搀攙", "谗讒", "馋饞", "缠纏", "忏懺", "偿償", "厂廠", "彻徹", "尘塵", "衬襯",
    "称稱", "惩懲", "迟遲", "冲衝", /*丑醜 is handled in special case */ "出齣", "础礎", "处處", "触觸", "辞辭",
    "聪聰", "丛叢", "担擔", "胆膽", "导導", "灯燈", "邓鄧", "敌敵", "籴糴", "递遞",
    "点點", "淀澱", "电電", "冬鼕", "斗鬥", "独獨", "吨噸", "夺奪", "堕墮", "儿兒",
    "矾礬", "范範", "飞飛", "坟墳", "奋奮", "粪糞", "凤鳳", "肤膚", "妇婦", "复復複",
    "盖蓋", "干乾幹",       "赶趕", "个個", "巩鞏", "沟溝", "构構", "购購", "谷榖",
    "顾顧", "刮颳", "关關", "观觀", "柜櫃", "汉漢", "号號", "合閤", "轰轟", "后後",
    "胡鬍", "壶壺", "沪滬", "护護", "划劃", "怀懷", "坏壞", "欢歡", "环環", "还還",
    "回迴", "伙夥", "获獲穫",       "击撃", "鸡鷄", "积積", "极極", "际際", "继繼",
    "家傢", "价價", "艰艱", "歼殲", "茧繭", "拣揀", "硷鹸", "舰艦", "姜薑", "浆漿",
    "桨槳", "奖奬", "讲講", "酱醤", "胶膠", "阶階", "疖癤", "洁潔", "借藉", "仅僅",
    "惊驚", "竞競", "旧舊", "剧劇", "据據", "惧懼", "卷捲", "开開", "克剋", "垦墾",
    "恳懇", "夸誇", "块塊", "亏虧", "困睏", "腊臘", "蜡蠟", "兰蘭", "拦攔", "栏欄",
    "烂爛", "累纍", "垒壘", "类類", "里裏", "礼禮", "隶隷", "帘簾", "联聯", "怜憐",
    "炼煉", "练練", "粮糧", "疗療", "辽遼", "了瞭", "猎獵", "临臨", "邻鄰", "岭嶺",
    "庐廬", "芦蘆", "炉爐", "陆陸", "驴驢", "乱亂", "么麽", "霉黴", "蒙矇濛懞",
    "梦夢", "面麵", "庙廟", "灭滅", "蔑衊", "亩畝", "恼惱", "脑腦", "拟擬", "酿釀",
    "疟瘧", "盘盤", "辟闢", "苹蘋", "凭憑", "扑撲", "仆僕", "朴樸", "启啓", "签籤",
    "千韆", "牵牽", "纤縴纖",       "窍竅", "窃竊", "寝寢", "庆慶", "琼瓊", "秋鞦",
    "曲麯", "权權", "劝勸", "确確", "让讓", "扰擾", "热熱", "认認", "洒灑", "伞傘",
    "丧喪", "扫掃", "涩澀", "晒曬", "伤傷", "舍捨", "沈瀋", "声聲", "胜勝", "湿濕",
    "实實", "适適", "势勢", "兽獸", "书書", "术術", "树樹", "帅帥", "松鬆", "苏蘇囌",
    "虽雖", "随隨", "台臺檯颱",     "态態", "坛壇罎",       "叹嘆", "誊謄", "体體",
    "粜糶", "铁鐵", "听聽", "厅廳", "头頭", "图圖", "涂塗", "团團糰",       "椭橢",
    "洼窪", "袜襪", "网網", "卫衛", "稳穩", "务務", "雾霧", "牺犧", "习習", "系係繫",
    "戏戲", "虾蝦", "吓嚇", "咸鹹", "显顯", "宪憲", "县縣", "响響", "向嚮", "协協",
    "胁脅", "亵褻", "衅釁", "兴興", "须鬚", "悬懸", "选選", "旋鏇", "压壓", "盐鹽",
    "阳陽", "养養", "痒癢", "样様", "钥鑰", "药藥", "爷爺", "叶葉", "医醫", "亿億",
    "忆憶", "应應", "痈癰", "拥擁", "佣傭", "踊踴", "忧憂", "优優", "邮郵", "余餘",
    "御禦", "吁籲", "郁鬱", "誉譽", "渊淵", "园園", "远遠", "愿願", "跃躍", "运運",
    "酝醖", "杂雜", "赃臓", "脏贜髒",       "凿鑿", "枣棗", "灶竈", "斋齋", "毡氈",
    "战戰", "赵趙", "折摺", "这這", "征徵", "症癥", "证證", "只隻祗衹",     "致緻",
    "制製", "钟鐘鍾",       "肿腫", "种種", "众衆", "昼晝", "朱硃", "烛燭", "筑築",
    "庄莊", "桩樁", "妆妝", "装裝", "壮壯", "状狀", "准凖", "浊濁", "总總", "钻鑽",
    // 第二表: 可作简化偏旁用的简化字和简化偏旁
    "爱愛", "罢罷", "备備", "贝貝", "笔筆", "毕畢", "边邊", "宾賓", "参參", "仓倉",
    "产産", "长長", "尝嘗", "车車", "齿齒", "虫蟲", "刍芻", "从從", "窜竄", "达達",
    "带帶", "单單", "当當噹",       "党黨", "东東", "动動", "断斷", "对對", "队隊",
    "尔爾", "发發髮",       "丰豐", "风風", "冈岡", "广廣", "归歸", "龟龜", "国國",
    "过過", "华華", "画畫", "汇匯彙",       "会會", "几幾", "夹夾", "戋戔", "监監",
    "见見", "荐薦", "将將", "节節", "尽盡儘",       "进進", "举舉", "壳殻", "来來",
    "乐樂", "离離", "历歷曆",       "丽麗", "两兩", "灵靈", "刘劉", "龙龍", "娄婁",
    "卢盧", "虏虜", "卤鹵滷",       "录録", "虑慮", "仑侖", "罗羅", "马馬", "买買",
    "卖賣", "麦麥", "门門", "黾黽", "难難", "鸟鳥", "聂聶", "宁寧寜", "农農", "齐齊",
    "岂豈", "气氣", "迁遷", "佥僉", "乔喬", "亲親", "穷窮", "区區", "啬嗇", "杀殺",
    "审審", "圣聖", "师師", "时時", "寿壽夀", "属屬", "双雙", "肃肅", "岁嵗", "孙孫",
    "条條", "万萬", "为為", "韦韋", "乌烏", "无無", "献獻", "乡鄉", "写寫", "寻尋",
    "亚亞", "严嚴", "厌厭", "尧堯", "业業", "页頁", "义義", "艺兿", "阴陰", "隐隱",
    "犹猶", "鱼魚", "与與", "云雲", "郑鄭", "执執", "质質", "专專",
    // 第三表: 应用第二表所列简化字和简化偏旁得出来的简化字
    "嗳噯", "嫒嬡", "叆靉", "瑷璦", "暧曖", "摆擺襬",       "罴羆", "", "惫憊",
    "贞貞", "则則", "负負", "贡貢", "呗唄", "员員", "财財", "狈狽", "责責", "厕厠",
    "贤賢", "账賬", "贩販", "贬貶", "败敗", "贮貯", "贪貪", "贫貧", "侦偵", "侧側",
    "货貨", "贯貫", "测測", "浈湞", "恻惻", "贰貳", "贲賁", "贳貰", "费費", "郧鄖",
    "勋勛", "帧幀", "贴貼", "贶貺", "贻貽", "贱賤", "贵貴", "钡鋇", "贷貸", "贸貿",
    "贺賀", "陨隕", "涢溳", "资資", "祯禎", "贾賈", "损損", "贽贄", "埙塤", "桢楨",
    "唝嗊", "唢嗩", "赅賅", "圆圓", "贼賊", "贿賄", "赆贐", "赂賂", "债債", "赁賃",
    "渍漬", "惯慣", "琐瑣", "赉賚", "匮匱", "掼摜", "殒殞", "勚勩", "赈賑", "婴嬰",
    "啧嘖", "赊賒", "帻幘", "偾僨", "铡鍘", "绩績", "溃潰", "溅濺", "赓賡", "愦憒",
    "愤憤", "蒉蕢", "赍賫", "蒇蕆", "", "赔賠", "赕賧", "遗遺", "赋賦", "喷噴",
    "赌賭", "赎贖", "赏賞", "赐賜", "赒賙", "锁鎖", "馈饋", "赖賴", "赪赬", "碛磧",
    "㱮殨", "赗賵", "腻膩", "赛賽", "", "赘贅", "撄攖", "槚檟", "嘤嚶", "赚賺",
    "赙賻", "罂罌", "镄鐨", "箦簀", "鲗鰂", "缨纓", "璎瓔", "聩聵", "樱櫻", "赜賾",
    "篑簣", "濑瀨", "瘿癭", "懒懶", "赝贋", "豮豶", "赠贈", "鹦鸚", "獭獺", "赞贊",
    "赢贏", "赡贍", "癞癩", "攒攢", "籁籟", "缵纘", "瓒瓚", "臜臢", "赣贛", "趱趲",
    "躜躦", "戆戇", "滗潷", "荜蓽", "哔嗶", "筚篳", "跸蹕", "笾籩", "傧儐", "滨濱",
    "眬矓", "嫔嬪", "缤繽", "殡殯", "槟檳", "膑臏", "镔鑌", "髌髕", "鬓鬢", "渗滲",
    "惨慘", "掺摻", "骖驂", "毵毿", "瘆瘮", "碜磣", "穇", "糁糝", "伧傖", "创創",
    "沧滄", "怆愴", "苍蒼", "抢搶", "呛嗆", "炝熗", "玱瑲", "枪槍", "戗戧", "疮瘡",
    "鸧鶬", "舱艙", "跄蹌", "浐滻", "萨薩", "铲鏟", "伥倀", "怅悵", "帐帳", "张張",
    "枨棖", "账賬", "胀脹", "涨漲", "鲿鱨", "轧軋", "军軍", "轨軌", "厍厙", "阵陣",
    "库庫", "连連", "轩軒", "诨諢", "郓鄆", "轫軔", "轭軛", "匦匭", "转轉", "轮輪",
    "斩斬", "软軟", "浑渾", "恽惲", "砗硨", "轶軼", "轲軻", "轱軲", "轷軤", "轻輕",
    "轳轤", "轴軸", "挥揮", "荤葷", "轹轢", "轸軫", "轺軺", "涟漣", "珲琿", "载載",
    "莲蓮", "较較", "轼軾", "轾輊", "辂輅", "轿轎", "晕暈", "渐漸", "惭慚", "皲皸",
    "琏璉", "辅輔", "辄輒", "辆輛", "堑塹", "啭囀", "崭嶄", "裤褲", "裢褳", "辇輦",
    "辋輞", "辍輟", "辊輥", "椠槧", "辎輜", "暂暫", "辉輝", "辈輩", "链鏈", "翚翬",
    "辏輳", "辐輻", "辑輯", "输輸", "毂轂", "辔轡", "辖轄", "辕轅", "辗輾", "舆輿",
    "辘轆", "撵攆", "鲢鰱", "辙轍", "錾鏨", "辚轔", "龀齔", "啮嚙", "龆齠", "龅齙",
    "龃齟", "龄齡", "龇齜", "龈齦", "龉齬", "龊齪", "龌齷", "龋齲", "蛊蠱", "诌謅",
    "", "邹鄒", "", "驺騶", "绉縐", "皱皺", "趋趨", "雏雛", "苁蓯", "纵縱",
    "枞樅", "怂慫", "耸聳", "撺攛", "镩鑹", "蹿躥", "闼闥", "挞撻", "哒噠", "鞑韃",
    "滞滯", "郸鄲", "惮憚", "阐闡", "掸撣", "弹彈", "婵嬋", "禅禪", "殚殫", "瘅癉",
    "蝉蟬", "箪簞", "蕲蘄", "冁囅", "挡擋", "档檔", "裆襠", "铛鐺", "谠讜", "傥儻",
    "镋钂", "冻凍", "陈陳", "岽崬", "栋棟", "胨腖", "鸫鶇", "恸慟", "簖籪", "怼懟",
    "坠墜", "迩邇", "弥彌瀰",       "祢禰", "鑭玺壐", "猕獼", "泼潑", "废廢", "拨撥",
    "鏺", "沣灃", "艳艶", "滟灧", "讽諷", "沨渢", "岚嵐", "枫楓", "疯瘋", "飒颯",
    "砜碸", "飓颶", "飔颸", "飕颼", "飗飀", "飘飄", "飙飆", "刚剛", "掆", "岗崗",
    "纲綱", "棡", "钢鋼", "邝鄺", "圹壙", "扩擴", "犷獷", "纩纊", "旷曠", "矿礦",
    "岿巋", "阄鬮", "掴摑", "帼幗", "腘膕", "蝈蟈", "挝撾", "哗嘩", "骅驊", "烨燁",
    "桦樺", "晔曄", "铧鏵", "婳嫿", "擓","刽劊", "郐鄶", "侩儈", "浍澮", "荟薈",
    "哙噲", "狯獪", "绘繪", "烩燴", "桧檜", "脍膾", "鲙鱠", "讥譏", "叽嘰", "饥饑",
    "机機", "玑璣", "矶磯", "虮蟣", "郏郟", "侠俠", "陕陝", "浃浹", "挟挾", "荚莢",
    "峡峽", "狭狹", "惬愜", "硖硤", "铗鋏", "颊頰", "蛱蛺", "瘗瘞", "箧篋", "刬剗",
    "浅淺", "饯餞", "线綫", "残殘", "栈棧", "贱賤", "盏盞", "钱錢", "笺箋", "溅濺",
    "践踐", "滥濫", "蓝藍", "尴尷", "槛檻", "褴襤", "篮籃", "苋莧", "岘峴", "觃覎",
    "视視", "规規", "现現", "枧梘", "觅覓", "觉覺", "砚硯", "觇覘", "览覽", "宽寬",
    "蚬蜆", "觊覬", "笕筧", "觋覡", "觌覿", "靓靚", "搅攪", "揽攬", "缆纜", "窥窺",
    "榄欖", "觎覦", "觏覯", "觐覲", "觑覷", "髋髖", "鞯韉", "蒋蔣", "锵鏘", "栉櫛",
    "浕濜", "荩藎", "烬燼", "赆贐", "琎璡", "榉櫸", "悫慤", "涞淶", "莱萊", "崃峽",
    "徕徠", "赉賚", "睐睞", "铼錸", "泺濼", "烁爍", "栎櫟", "轹轢", "砾礫", "铄鑠",
    "漓灕", "篱籬", "沥瀝", "坜壢", "苈藶", "呖嚦", "枥櫪", "疬癧", "雳靂", "俪儷",
    "郦酈", "逦邐", "骊驪", "鹂鸝", "酾釃", "鲡鱺", "俩倆", "唡啢", "辆輛", "满滿",
    "瞒瞞", "颟顢", "螨蟎", "魉魎", "懑懣", "蹒蹣", "棂欞", "浏瀏", "陇隴", "泷瀧",
    "宠寵", "庞龐", "垄壟", "拢攏", "茏蘢", "咙嚨", "珑瓏", "栊櫳", "龑", "昽曨",
    "胧朧", "砻礱", "袭襲", "聋聾", "龚龔", "龛龕", "笼籠", "偻僂", "溇漊", "蒌蔞",
    "搂摟", "嵝嶁", "喽嘍", "缕縷", "屡屢", "数數", "楼樓", "瘘瘻", "褛褸", "窭窶",
    "瞜", "镂鏤", "屦屨", "蝼螻", "篓簍", "耧耬", "薮藪", "擞擻", "髅髏", "泸濾",
    "垆壚", "栌櫨", "轳轤", "胪臚", "鸬鸕", "颅顱", "舻艫", "鲈鱸", "掳擄", "鹾鹺",
    "箓籙", "滤濾", "摅攄", "论論", "伦倫", "沦淪", "抡掄", "囵圇", "纶綸", "轮輪",
    "瘪癟", "萝蘿", "啰囉", "逻邏", "猡玀", "椤欏", "锣鑼", "箩籮",
    "冯馮", "驭馭", "闯闖", "吗嗎", "犸獁", "驮馱", "驰馳", "驯馴", "妈媽", "玛瑪",
    "驱驅", "驳駁", "码碼", "驼駝", "驻駐", "驵駔", "驾駕", "驿驛", "驷駟", "驶駛",
    "驹駒", "驺騶", "骀駘", "驸駙", "驽駑", "骂駡", "蚂螞", "笃篤", "骇駭", "骈駢",
    "骁驍", "骄驕", "骅驊", "骆駱", "骊驪", "骋騁", "验驗", "骏駿", "骎駸", "骑騎",
    "骐騏", "骒騍", "骓騅", "骖驂", "骗騙", "骘騭", "骛騖", "骚騷", "骞騫", "骜驁",
    "蓦驀", "腾騰", "骝騮", "骟騸", "骠驃", "骢驄", "骡騾", "羁覊", "骤驟", "骥驥",
    "骧驤", "荬蕒", "读讀", "渎瀆", "续續", "椟櫝", "觌覿", "赎贖", "犊犢", "牍牘",
    "窦竇", "黩黷", "唛嘜", "麸麩", "闩閂", "闪閃", "们們", "闭閉", "闯闖",
    "问問", "扪捫", "闱闈", "闵閔", "闷悶", "闰閏", "闲閑", "间間", "闹閙", "闸閘",
    "钔鍆", "阂閡", "闺閨", "闻聞", "闼闥", "闽閩", "闾閭", "闿闓", "", "阁閣",
    "阀閥", "润潤", "涧澗", "悯憫", "阆閬", "阅閲", "阃閫", "阄鬮", "", "娴嫻",
    "阏閼", "阈閾", "阉閹", "阊閶", "阍閽", "阌閿", "阋鬩", "阐闡", "阎閻", "焖燜",
    "阑闌", "裥襇", "阔闊", "痫癇", "鹇鷳", "阕闋", "阒闃", "搁擱", "锏鐧", "锎鐦",
    "阙闕", "阖闔", "阗碴", "榈櫚", "简簡", "谰讕", "阚闞", "蔺藺", "澜瀾", "斓斕",
    "镧鑭", "躏躪", "渑澠", "绳繩", "鼋黿", "蝇蠅", "鼍鼉", "傩儺", "滩灘",
    "摊擹", "瘫癱", "凫鳬", "鸠鳩", "岛島", "茑蔦", "鸢鳶",
    "鸣鳴", "枭梟", "鸩鴆", "鸦鴉", "鳾", "鸥鷗", "鸨鴇", "鸧鶬", "窎窵", "莺鶯",
    "鸪鴣", "捣搗", "鸫鶇", "鸬鸕", "鸭鴨", "鸯鴦", "鸮鴞", "鸲鴝", "鸰鴒", "鸳鴛",
    "鸵鴕", "袅裊", "鸱鴟", "鸶鷥", "鸾鵉", "鵁", "鸿鴻", "鸷鷙", "鸸鴯", "鴷",
    "鸼鵃", "鸽鴿", "鸹鴰", "鸺鵂", "鸻鴴", "鹈鵜", "鹇鷳", "鹁鵓", "鹂鸝", "鹃鵑",
    "鹆鵒", "鹄鵠", "鹅鵝", "鹑鶉", "鹒鶊", "鶄", "鹉鵡", "鹊鵲", "鹋鶓", "鹌鵪",
    "鹏鵬", "鹐鵮", "鹚鷀", "鹕鶘", "鹖鶡", "鶪", "鹗鶚", "鹘鶻", "鹙鶖", "鹜鶩",
    "鹛鶥", "鹤鶴", "鹣鶼", "鹞鷂", "鹡鶺", "鷉", "鹧鷓", "鹥鷖", "鹦鸚", "鹨鷚",
    "鹫鹫", "鹩鷯", "鹪鷦", "鹬鷸", "鹰鷹", "鹯鸇", "鹭鷺", "鸊", "鹳鸛", "慑攝",
    "滠灄", "摄攝", "嗫囁", "镊鑷", "颞顳", "蹑躡", "泞濘", "拧擰", "咛嚀", "狞獰",
    "柠檸", "聍聹", "侬儂", "浓濃", "哝噥", "脓膿", "剂劑", "侪儕", "济濟", "荠薺",
    "挤擠", "脐臍", "蛴蠐", "跻蠐", "霁霽", "鲚鱭", "齑齏", "剀剴", "凯凱", "恺愷",
    "闿闓", "垲塏", "桤榿", "觊覬", "硙磑", "皑皚", "铠鎧", "忾愾", "饩餼", "跹躚",
    "剑劍", "俭儉", "险險", "捡撿", "猃獫", "验驗", "检檢", "殓殮", "敛斂", "脸臉",
    "裣襝", "睑瞼", "签簽", "潋瀲", "蔹蘞", "侨僑", "挢撟", "荞蕎", "峤嶠", "骄驕",
    "娇嶠", "桥橋", "轿轎", "硚礄", "矫矯", "鞒鞽", "榇櫬", "藭", "讴謳", "伛傴",
    "沤漚", "怄慪", "抠摳", "奁奩", "呕嘔", "岖嶇", "妪嫗", "驱驅", "枢樞", "瓯甌",
    "欧歐", "殴毆", "鸥鷗", "眍瞘", "躯軀", "蔷薔", "墙墻", "嫱嬙", "樯檣", "穑穡",
    "铩鎩", "谉讅", "婶嬸", "柽檉", "蛏蟶", "浉溮", "狮獅", "蛳螄", "筛篩", "埘塒",
    "莳蒔", "鲥鰣", "俦儔", "涛濤", "祷禱", "焘燾", "畴疇", "铸鑄", "筹籌", "踌躊",
    "嘱囑", "瞩矚", "", "萧蕭", "啸嘯", "潇瀟", "箫簫", "蟏蠨", "刿劌", "哕噦",
    "秽穢", "荪蓀", "狲猻", "逊遜", "涤滌", "绦縧", "鲦鰷", "厉厲", "迈邁", "励勵",
    "疠癘", "虿蠆", "趸躉", "砺礪", "粝糲", "蛎蠣", "伪僞", "沩溈", "妫媯", "讳諱",
    "伟偉", "闱闈", "违違", "苇葦", "韧韌", "帏幃", "围圍", "纬緯", "炜煒", "祎禕",
    "玮瑋", "韨韍", "涠潿", "韩韓", "韫韞", "韪韙", "韬韜", "邬鄔", "坞塢", "呜嗚",
    "钨鎢", "怃憮", "庑廡", "抚撫", "芜蕪", "呒嘸", "妩嫵", "谳讞", "芗薌", "飨饗",
    "泻瀉", "浔潯", "荨蕁", "挦撏", "鲟鱘", "垩堊", "垭埡", "挜掗", "哑啞", "娅婭",
    "恶惡噁",       "氩氬", "壶壺", "俨儼", "酽釅", "恹懨", "厣厴", "靥靨", "餍饜",
    "魇魘", "黡黶", "侥僥", "浇澆", "挠撓", "荛蕘", "峣嶢", "哓嘵", "娆嬈", "骁驍",
    "绕繞", "饶饒", "烧焼", "桡橈", "晓曉", "硗磽", "铙鐃", "翘翹", "蛲蟯", "跷蹺",
    "邺鄴", "顶頂", "顷頃", "项項", "顸頇", "顺順", "须須", "颃頏", "烦煩", "顼瑣",
    "顽頑", "顿頓", "颀頎", "颁頒", "颂頌", "倾傾", "预預", "庼廎", "硕碩", "颅顱",
    "领領", "颈頸", "颇頗", "颏頦", "颊頰", "颉頡", "颍潁", "颌頜", "颋頲", "滪澦",
    "颐頤", "蓣蕷", "频頻", "颓頽", "颔頷", "颖穎", "颗顆", "额額", "颜顔", "撷擷",
    "题題", "颙顒", "颛顓", "缬纈", "濒瀕", "颠顛", "颟顢", "颞顳", "颡顙", "嚣囂",
    "颢顥", "颤顫", "巅巔", "颥顬", "癫癲", "灏灝", "颦顰", "颧顴", "议議", "仪儀",
    "蚁蟻", "呓囈", "荫蔭", "瘾癮", "莸蕕", "鱽魛", "渔漁", "鲂魴", "鱿魷", "鲁魯",
    "鲎鱟", "蓟薊", "鲆鮃", "鲏鮍", "鲅鮁", "鲈鱸", "鲇點", "鲊鮓", "鮣", "稣穌",
    "鲋鮒", "鲍鮑", "鲐鮐", "鲞鯗", "鲝鮺", "鲚鱭", "鲛鮫", "鲜鮮", "鲑鮭", "鲒鮚",
    "鲟鱘", "鲔鮪", "鲟鱘", "鲗鰂", "鲖鮦", "鲙鱠", "鲨鯊", "噜嚕", "鲡鱺", "鲠鯁",
    "鲢鰱", "鲫鯽", "鲥鰣", "鲩鯇", "鲣鰹", "鲤鯉", "鲦鰷", "鲧鯀", "橹櫓", "氇氌",
    "鲸鯨", "鲭鯖", "鲮鯪", "鲰鯫", "鲲鯤", "鲻鯔", "鲳鯧", "鲱鯡", "鲵鯢", "鲷鯛",
    "鲶鯰", "藓蘚", "鰌", "鰆", "鲿鱨", "鳊鯿", "鲽鰈", "鳁鰛", "鳃鰓", "鳄鰐",
    "镥鑥", "鳅鰍", "鳆鰒", "鳇鰉", "鳌鰲", "", "鰧", "鳒鰜", "鳍鰭", "鳎鰨",
    "鳏鰥", "鳑鰟", "癣癬", "鳖鱉", "鳙鱅", "鳛鰼", "鳕鱈", "鳔鰾", "鳓鰳", "鳘鰵",
    "鳗鰻", "鳝鱔", "鳟鱒", "鳞鱗", "鳜鱖", "鳣鱣", "鳢鱧", "屿嶼", "欤歟", "芸蕓",
    "昙曇", "叆靉", "叇靆", "掷擲", "踯躑", "垫墊", "挚摯", "贽贄", "鸷鷙", "蛰蟄",
    "絷縶", "锧鑕", "踬躓", "传傳", "抟摶", "转轉", "膞", "砖磚", "啭囀",
    "计計", "订訂", "讣訃", "讥譏", "议議", "讨討", "讧訌", "讦訐", "记記", "讯訊",
    "讪訕", "训訓", "讫訖", "访訪", "讶訝", "讳諱", "讵詎", "讴謳", "诀訣", "讷訥",
    "设設", "讽諷", "讹訛", "訢", "许許", "论論", "讼訟", "讻訩", "诂詁", "诃訶",
    "评評", "诏詔", "词詞", "译譯", "诎詘", "诇詗", "诅詛", "识識", "诌謅", "诋詆",
    "诉訴", "诈詐", "诊診", "诒詒", "诨諢", "该該", "详詳", "诧詫", "诓誆", "诖詿",
    "诘詰", "诙詼", "试試", "诗詩", "诩詡", "诤諍", "诠詮", "诛誅", "诔誄", "诟詬",
    "诣詣", "话話", "诡詭", "询詢", "诚誠", "诞誕", "浒滸", "诮誚", "说説", "诫誡",
    "诬誣", "语語", "诵誦", "罚罸", "误誤", "诰誥", "诳誑", "诱誘", "诲誨", "诶誒",
    "狱獄", "谊誼", "谅諒", "谈談", "谆諄", "谉讅", "谇誶", "请請", "诺諾", "诸諸",
    "读讀", "诼諑", "诹諏", "课課", "诽誹", "诿諉", "谁誰", "谀諛", "调調", "谄諂",
    "谂諗", "谛諦", "谙諳", "谜謎", "谚諺", "谝諞", "谘諮", "谌諶", "谎謊", "谋謀",
    "谍諜", "谐諧", "谏諫", "谞諝", "谑謔", "谒謁", "谔諤", "谓謂", "谖諼", "谕諭",
    "谥謚", "谤謗", "谦謙", "谧謐", "谟謨", "谠讜", "谡謖", "谢謝", "谣謡", "储儲",
    "谪謫", "谫譾", "谨謹", "谬謬", "谩謾", "谱譜", "谮譖", "谭譚", "谰讕", "谲譎",
    "谯譙", "蔼藹", "槠櫧", "谴譴", "谵譫", "谳讞", "辩辯", "讌", "雠讎", "谶讖",
    "霭靄", "饥饑", "饦飥", "饧餳", "饨飩", "饭飯",
    "饮飲", "饫飫", "饩餼", "饪飪", "饬飭", "饲飼", "饯餞", "饰飾", "饱飽", "饴飴",
    "饳飿", "饸餄", "饷餉", "饺餃", "饻餏", "饼餠", "饵餌", "饶饒", "蚀蝕", "饹餎",
    "饽餑", "馁餒", "饿餓", "馆館", "馄餛", "馃餜", "馅餡", "馉餶", "馇餷", "馈饋",
    "馊餿", "馐饈", "馍饃", "馎餺", "馏餾", "馑饉", "馒饅", "馓饊", "馔饌", "馕饟",
    "汤湯", "扬揚", "场場", "旸暘", "饧餳", "炀煬", "杨楊", "肠腸", "疡瘍", "砀碭",
    "畅暢", "钖錫", "殇殤", "荡蕩", "烫燙", "觞觴",
    "丝絲", "纠糾", "纩纊", "纡紆", "纣紂", "红紅", "纪紀", "纫紉", "纥紇", "约约",
    "纨紈", "级級", "纺紡", "纹紋", "纬緯", "纭紜", "纯純", "纰紕", "纽紐", "纳納",
    "纲綱", "纱紗", "纴紝", "纷紛", "纶綸", "纸紙", "纵縱", "纾紓", "纼紖", "咝噝",
    "绊絆", "线綫", "绀紺", "绁紲", "绂紱", "绋紼", "绎繹", "经經", "绍紹", "组組",
    "细細", "紬", "绅紳", "织織", "绌絀", "终終", "绉縐", "绐紿", "哟喲", "绖絰",
    "荮葤", "荭葒", "绞絞", "统統", "绒絨", "绕繞", "绔絝", "结結", "绗絎", "给給",
    "绘繪", "绝絶", "绛絳", "络絡", "绚絢", "绑綁", "莼蒓", "绠綆", "绨綈", "绡綃",
    "绢絹", "绣綉", "绥綏", "绦縧", "鸶鷥", "综綜", "绽綻", "绾綰", "绻綣", "绩績",
    "绫綾", "绪緒", "续續", "绮綺", "缀綴", "绿緑", "绰綽", "绲緄", "绳繩", "绯緋",
    "绶綬", "绸綢", "绷綳", "绺綹", "维維", "绵綿", "缁緇", "缔締", "编編", "缕縷",
    "缃緗", "缂緙", "缅緬", "缘緣", "缉緝", "缇緹", "缈緲", "缗緡", "缊緼", "缌緦",
    "缆纜", "缓緩", "缄緘", "缑緱", "缒縋", "缎緞", "辔轡", "缞縗", "缤繽", "缟縞",
    "缣縑", "缢縊", "缚縛", "缙縉", "缛縟", "缜縝", "缝縫", "缡縭", "潍濰", "缩縮",
    "缥縹", "缪繆", "缦縵", "缨纓", "缫繅", "缧縲", "蕴藴", "缮繕", "缯繒", "缬纈",
    "缭繚", "橼櫞", "缰繮", "缳繯", "缲繰", "缱繾", "缴繳", "辫辮", "缵纘", "坚堅",
    "贤賢", "肾腎", "竖竪", "悭慳", "紧緊", "铿鏗", "鲣鰹", "劳勞", "茕煢", "茔塋",
    "荧熒", "荣榮", "荥滎", "荦熒", "涝澇", "崂嶗", "莹瑩", "捞撈", "唠嘮", "莺鶯",
    "萤螢", "营營", "萦縈", "痨癆", "嵘嶸", "铹鐒", "耢耮", "蝾蠑", "览覧", "揽攬",
    "缆纜", "榄欖", "鉴鑒", "识識", "帜幟", "织織", "炽熾", "职職",
    "钆釓", "钇釔", "钌釕", "钋釙", "钉釘", "针針", "钊釗", "钗釵", "钎釺", "钓釣",
    "钏釧", "钍釷", "钐釤", "钒釩", "钖鍚", "钕釹", "钔鍆", "钬鈥", "钫鈁", "钚鈈",
    "釾", "钪鈧", "钯鈀", "钭鈄", "钙鈣", "钝鈍", "钛鈦", "钘鈃", "钮鈕", "钞鈔",
    "钢鋼", "钠鈉", "钡鋇", "钤鈐", "钧鈞", "钩鈎", "钦欽", "钨鎢", "铋鉍", "钰鈺",
    "钱錢", "钲鉦", "钳鉗", "钴鈷", "钺鉞", "钵鉢", "钹鈸", "钼鉬", "钾鉀", "铀鈾",
    "钿鈿", "铎鐸", "鏺", "铃鈴", "铅鉛", "铂鉑", "铄鑠", "铆鉚", "铍鈹", "钶鈳",
    "铊鉈", "钽鉭", "铌鈮", "钷鉕", "铈鈰", "铉鉉", "铒鉺", "铑銠", "铕銪", "铟銦",
    "铷銣", "铯銫", "铥銩", "铪鉿", "铞銱", "铫銚", "铵銨", "衔銜", "铲鏟", "铰鉸",
    "铳銃", "铱銥", "铓鋩", "铗鋏", "铐銬", "铏鉶", "铙鐃", "银銀", "铛鐺", "铜銅",
    "铝鋁", "铡鍘", "铠鎧", "铨銓", "铢銖", "铣銑", "铤鋌", "铭銘", "铬鉻", "铮錚",
    "铧鏵", "铩鎩", "揿撳", "锌鋅", "锐鋭", "锑銻", "锒鋃", "铺鋪", "铸鑄", "嵚嶔",
    "锓鋟", "锃鋥", "链鏈", "铿鏗", "锏鐧", "销銷", "锁鎖", "锄鋤", "锅鍋", "锉銼",
    "锈銹", "锋鋒", "锆鋯", "铹鐒", "锔鋦", "锕錒", "锎鐦", "铽鋱", "铼錸", "锇鋨",
    "锂鋰", "锧鑕", "锘鍩", "锞錁", "锭錠", "锗鍺", "锝鍀", "锫錇", "错錯", "锚錨",
    "锛錛", "锯鋸", "锰錳", "锢錮", "锟錕", "锡錫", "锣鑼", "锤錘", "锥錐", "锦錦",
    "锨鍁", "锱錙", "键鍵", "镀鍍", "镃鎡", "镁鎂", "镂鏤", "锲鍥", "锵鏘", "锷鍔",
    "锶鍶", "锴鍇", "锾鍰", "锹鍬", "锿鎄", "镅鎇", "镄鐨", "锻鍛", "锸鍤", "锼鎪",
    "镎鎿", "镓鎵", "镋钂", "镔鑌", "镒鎰", "", "镑鎊", "镐鎬", "镉鎘", "镊鑷",
    "镇鎮", "镍鎳", "镌鎸", "镏鎦", "镜鏡", "镝鏑", "镛鏞", "镞鏃", "镖鏢", "镚鏰",
    "镗鏜", "鐯", "镘鏝", "镩鑹", "镦鐓", "鐥", "镨鐠", "镧鑭", "镥鑥", "镤鏷",
    "镢鐝", "镣鐐", "镫鐙", "镪鏹", "镰鐮", "镱鐿", "镭鐳", "镬鑊", "镮鐶", "镯鐲",
    "镲鑔", "镳鑣", "镴鑞", "镶鑲", "钁", "峃嶨", "学學", "觉覺", "搅攪", "喾嚳",
    "鲎鱟", "黉黌", "译譯", "泽澤", "怿懌", "择擇", "峄嶧", "绎繹", "驿驛", "铎鐸",
    "萚蘀", "释釋", "箨籜", "劲勁", "刭剄", "陉陘", "泾涇", "茎莖", "径徑", "经經",
    "烃烴", "轻輕", "氢氫", "胫脛", "痉痙", "羟羥", "颈頸", "巯巰", "变變", "弯彎",
    "孪孿", "峦巒", "娈孌", "恋戀", "栾欒", "挛攣", "鸾鸞", "湾灣", "蛮蠻", "脔臠",
    "滦灤", "銮鑾", "剐剮", "涡渦", "埚堝", "喎", "莴萵", "娲媧", "祸禍", "脶腡",
    "窝窩", "锅鍋", "蜗蝸",
    // 附录
    "呆獃騃",      "布佈", "痴癡", "床牀", "唇脣", "雇僱", "挂掛", "哄閧鬨",
    "迹跡蹟",      "秸稭", "杰傑", "巨鉅", "昆崑崐",  "捆綑", "泪淚", "厘釐", "麻蔴",
    "脉脈", "猫貓", "栖棲", "弃棄", "升昇阩", "笋筍", "它牠", "席蓆", "凶兇", "绣繡",
    "锈鏽", "岩巖", "异異", "涌湧", "岳嶽", "韵韻", "灾灾", "札剳劄", "扎紥紮", "占佔",
    "周週", "注註",

    // Following characters are not covered by 简化字总表.
    "黄黃", "吴吳", "晋晉", "岁歲"
  )

  /* 简化字总表（1986年新版）.
   *
   * For word (multiple characters). The main intention is to handle
   * exceptions and place names, as word will be handled first.
   *
   * For each element, the first part is Simplified Chinese, and
   * the second part is correponding Traditional Chinese.
   */
  private val WordTable = List(
    // 第一表: 不作简化偏旁用的简化字
    ("乾坤", "乾坤"), ("乾隆", "乾隆"), ("乾明", "乾明"), ("乾封", "乾封"),
    ("乾元", "乾元"), ("乾符", "乾符"), ("乾宁", "乾寧"), ("乾化", "乾化"),
    ("乾祐", "乾祐"), ("乾德", "乾德"), ("乾兴", "乾興"), ("乾亨", "乾亨"),
    ("乾统", "乾統"),
    ("慰藉", "慰藉"), ("狼藉", "狼藉"),
    ("幺麽小丑", "幺麽小丑"),           ("馀年无多", "馀年无多"),
    ("宫商角徵羽", "宫商角徵羽"),
    // 附录
    ("铁力", "铁骊"), ("爱辉", "瑷珲"), ("门源", "亹源"), ("和田", "和阗"),
    ("于田", "于阗"), ("若羌", "婼羌"), ("于都", "雩都"), ("大余", "大庾"),
    ("全南", "虔南"), ("新干", "新淦"), ("新余", "新喻"), ("波阳", "鄱阳"),
    ("寻乌", "寻邬"), ("玉林", "鬰林"), ("丰都", "酆都"), ("石柱县", "石砫縣"),
    ("越西", "越嶲"), ("甘洛", "呷洛"), ("务川", "婺川"), ("习水", "鳛水"),
    ("商洛", "商雒"), ("周至", "盩厔"), ("眉县", "郿縣"), ("礼泉", "醴泉"),
    ("郃阳", "合阳"), ("户县", "鄠縣"), ("洛南", "雒南"), ("彬县", "邠縣"),
    ("富县", "鄜縣"), ("佳县", "葭縣"), ("勉县", "沔縣"), ("旬邑", "栒邑"),
    ("旬阳", "洵阳"), ("千阳", "汧阳"), ("沈阳", "瀋阳"), ("浚县", "濬縣"),

    // All below is not in 简化字总表, and the purpose is to handle special/exceptional cases.

    // Special handling for 丑醜, as they are simplified in one character 丑
    ("丑陋", "醜陋"), ("丑化", "醜化"), ("丑恶", "醜惡"), ("丑闻", "醜聞"),
    ("丑态百出", "醜態百出"), ("跳梁小丑", "跳梁小醜"),

    // 年號
    ("周共和", "周共和"), ("周宣王", "周宣王"), ("周幽王", "周幽王"), ("周平王", "周平王"),
    ("周桓王", "周桓王"), ("周庄王", "周莊王"), ("周釐王", "周釐王"), ("周惠王", "周惠王"),
    ("周襄王", "周襄王"), ("周顷王", "周頃王"), ("周匡王", "周匡王"), ("周定王", "周定王"),
    ("周简王", "周簡王"), ("周灵王", "周靈王"), ("周景王", "周景王"), ("周敬王", "周敬王"),
    ("周元王", "周元王"), ("周贞定王", "周貞定王"), ("周考王", "周考王"), ("周威烈王", "周威烈王"),
    ("周安王", "周安王"), ("周烈王", "周烈王"), ("周显王", "周顯王"), ("周慎靓王", "周慎靚王"),
    ("周赧王", "周赧王"),
    ("汉高后", "漢高后"), ("北周", "北周"), ("唐武后", "唐武后"),
    ("晋出帝", "晉出帝"), ("后周", "後周"),
    ("庆历", "慶曆"),
    ("元乃马真后", "元乃马真后"), ("元海迷失后", "元海迷失后"),
    ("征和", "征和"),
    ("咸熙", "咸熙"), ("咸宁", "咸寧"), ("咸和", "咸和"), ("咸康", "咸康"),
    ("咸安", "咸安"), ("咸亨", "咸亨"), ("咸通", "咸通"), ("咸平", "咸平"),
    ("咸淳", "咸淳"), ("咸雍", "咸雍"), ("咸清", "咸清"), ("咸丰", "咸豐"),
    ("致和", "致和"), ("万历", "萬曆"),

    // Following is from http://www.zhihu.com/question/29199314, http://zh.m.wikipedia.org/wiki/簡繁轉換一對多列表
    ("云云", "云云")
  )

  private var simplified2TraditionalCharacterMap = new mutable.HashMap[Char, Char]()
  private var traditional2SimplifiedCharacterMap = new mutable.HashMap[Char, Char]()
  for (charPair <- CharacterTable) {
    val simplified = charPair(0)
    assert(charPair.length >= 2)
    simplified2TraditionalCharacterMap(simplified) = charPair(1)
    for (traditional <- charPair.substring(1)) {
      traditional2SimplifiedCharacterMap(traditional) = simplified
    }
  }

  private var simplified2TraditionalWordMap = new mutable.HashMap[String, String]()
  private var traditional2SimplifiedWordMap = new mutable.HashMap[String, String]()
  private var maxWordLength = 2
  for (wordPair <- WordTable) {
    val (simplified, traditional) = wordPair
    assert(simplified.length == traditional.length)
    simplified2TraditionalWordMap(simplified) = traditional
    traditional2SimplifiedWordMap(traditional) = simplified

    // Keep the identity relationship.
    simplified2TraditionalWordMap(traditional) = traditional
    traditional2SimplifiedWordMap(simplified) = simplified

    if (simplified.length > maxWordLength) {
      maxWordLength = simplified.length
    }
  }

  private val simplified2TraditionalMaps =
    (simplified2TraditionalCharacterMap, simplified2TraditionalWordMap)
  private val traditional2SimplifiedMaps =
    (traditional2SimplifiedCharacterMap, traditional2SimplifiedWordMap)
}
