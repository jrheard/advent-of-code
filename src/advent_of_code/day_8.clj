(ns advent-of-code.day-8
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.string :refer [split]]
            [clojure.edn :as edn]))

(def raw-input "\"\\xa8br\\x8bjr\\\"\"\n\"nq\"\n\"zjrfcpbktjmrzgsz\\xcaqsc\\x03n\\\"huqab\"\n\"daz\\\\zyyxddpwk\"\n\"draes\\xa2n\\\\g\\x27ek\\\"lj\\\"\\\\viqych\"\n\"nnx\\\\krnrfomdnt\\x2flbl\\xd2xpo\\\"cp\\\"k\"\n\"kwdaapalq\"\n\"u\\\"ptk\"\n\"ckhorczuiudfjmmcc\\\\u\\\"wozqxibsfjma\"\n\"ydctdrxat\\\"pd\\\"lwi\\\"bjesevfw\\xe8\"\n\"v\\\"\\xa8rrzep\\\"\\\"r\"\n\"nbydghkfvmq\\\\\\xe0\\\"lfsrsvlsj\\\"i\\x61liif\"\n\"jsas\\\"u\"\n\"odipikxlo\"\n\"\\\"rnubsgwltqkbsu\\\"pcpcs\"\n\"eitk\\\\f\\\\mhcqqoym\\\\ji\"\n\"vnedc\"\n\"\\\"lhcaurdqzyjyu\"\n\"haxzsa\\\"zcn\\\"y\\\"foclgtjfcnv\\\"m\\x68krc\"\n\"\\\"eoeggg\\\"tmiydvcay\\\"vfavc\"\n\"snqvyqoncwxcvwbdktoywch\"\n\"rnfgjsyr\\xd5wacy\"\n\"ik\\\"hebrpvsts\"\n\"txw\"\n\"\\x15pxtdkogd\\\"urbm\\\"gevhh\\\"nxr\\x3erxtk\"\n\"cetqtcy\"\n\"inleep\\\\mgl\"\n\"uflwbxvww\\x2cxzezqnaply\\\"yh\\\"qlllzk\"\n\"eepak\\\"xqtedzt\"\n\"na\\x61qzfieafvyrsnwkssznohjmc\"\n\"yceaonylz\\xc1\\\\jrlbbkzwsidfi\"\n\"ybqafngkcqpbp\"\n\"\\xaft\"\n\"yidjpaobqydso\"\n\"ju\\\\ldxig\\\\lrdrhjcmm\\x77rc\"\n\"tylacqeslnwj\\x48ds\\\"tjxa\"\n\"efbfm\"\n\"\\\\fxkgoprgdcjgyajykg\\\\dtbrz\"\n\"eujvva\"\n\"h\\x7acwfpikme\\\\vwthyvrqdnx\\\"\"\n\"rbpbrxm\\\\\\\"\\\"\\\"voxx\"\n\"ykiw\\\"tkb\\\\lforu\\\"rsf\\\\tf\\\"x\\\"rqti\"\n\"e\\\\wh\\x77aqeugiq\\\\ihhfqfuaij\"\n\"g\\\"t\\\\o\"\n\"nxzo\\\"hf\\\\xp\"\n\"dxiaqfo\\xea\"\n\"kali\\\\zczhiqkqzybjj\\\"fgdjnik\"\n\"zdkgrqmdv\"\n\"bimxim\\xb6lrwsaj\\\"ui\\\"a\"\n\"\\\"rrznitibgx\\\\olpsjmjqzctxaubdifsq\"\n\"zb\\\"khzixaacmhuzmlymoformipdzml\"\n\"qfwi\"\n\"hjwsxfpphttjy\\\"\\\"zixais\\xbblgnqfto\"\n\"puj\\\\qmyu\\\"nqgaqfthbwjokbmrpbhpi\"\n\"cyxdpkh\\\\\\\"\"\n\"q\"\n\"m\"\n\"tbxdzzllarlo\"\n\"gbtys\"\n\"gytilk\\\\vlqxvcuutjunrqc\"\n\"uugkvcuzan\\\\eyhb\"\n\"yaxr\\\"genlbgw\\\"\\\\uc\"\n\"nrgecjeip\\\\sjdvgqaqxwsqactopu\"\n\"pu\\\"r\\\"txpyrkfny\\\\zmwfneyvwmnkkdipv\"\n\"jm\\xa3bhwvq\"\n\"qxojmnml\\\"w\\x9airr\"\n\"xbzsuihs\\x4dcedy\\xaclrhgii\\\\\\\"\"\n\"drgjirusrekrwmvxllwdm\"\n\"\\x28hfxnfpycmpnkku\\\"csuf\\xaarxlqyg\\\"x\"\n\"\\\"zvz\\\\rmg\\\"\\\\sxxoifffyqfyn\\\"iq\\\"ps\"\n\"\\\"z\"\n\"zbwkmk\\\"sgzos\\x93gtc\\\"\"\n\"bvm\\x28aa\\\\\\\\\\\"pywuhaniox\\\\z\\\\hbp\\xd7mold\"\n\"aszgvsyna\"\n\"qf\\\"vdwuss\"\n\"lnohni\\\"qwiacjsjegstlbfq\\\\kyjhyd\"\n\"c\\\\naawulxlqplnacvytspry\\xf5ytxxqq\"\n\"razwqmsqgbaaxcd\\\\f\"\n\"radggyrjrg\\\"zx\"\n\"\\\"pu\\x11t\\\\ajcjuieinlkvya\"\n\"veggiskh\"\n\"eglfhjxiet\\\"kouqfskwsy\\\"hpthsldel\"\n\"mv\\xc1b\\\"f\\\\shrssnjwcpmurepdxdlcj\"\n\"dlayjd\\\"suvzotgdtc\"\n\"\\xa9pvxeopn\"\n\"lpplsaxy\\\"oiwaq\"\n\"hqwh\\\\lusv\"\n\"hykykwlx\\\"\\xa5atkgh\\\\d\\x63dff\"\n\"vfktanpjy\\\"xxetc\"\n\"dnhwkgjnsmsswfuelvihvjl\\\"jtf\"\n\"x\\\"dwvzra\\\"nbbsewftehczgbvfzd\\\"rau\"\n\"csfi\\\"mzejnjqkqupwadrgti\\\"von\"\n\"xckf\\xf7xsm\\\\pgvlpetjndpyblais\\\\z\"\n\"yecy\\x6fuj\\x58bwpgeuiw\\\"mdu\"\n\"fgb\"\n\"c\\\\lx\\x3efthet\\xfdelgvwvpem\"\n\"kgyrmarvfwjinlowt\"\n\"yzte\"\n\"vc\\\"z\"\n\"sxevqfzmmdwsuu\\\"\"\n\"fxbaercmcy\\xb6md\"\n\"f\"\n\"m\\x44gqbcppho\\\\b\"\n\"gtafr\\x57m\\x11jy\\\"\\\"erwmmpiwjkbckuw\"\n\"ufdjt\\\"kssprzxqixzxmq\\x58q\"\n\"yzbyo\\\"lfdbyaxexyfbnyv\\\\\\xe8xmre\"\n\"u\\x43ntr\\\\\\\\byyfjr\\\"iveujvnwsqbnpuvrta\"\n\"us\\xf6bai\"\n\"c\\\\edh\"\n\"tzckolphexfq\\\\\\x23\\xfbdqv\\\\\\\"m\"\n\"yjafhbvhhj\\x1b\\\"bplb\"\n\"\\\"o\"\n\"rubahvmp\\\"\"\n\"qmkukrnrmqumh\"\n\"wdpxyvyidhwjf\\\\nabbijwhr\\xc5bksvy\\\"p\"\n\"u\\\"prlpg\\\"\"\n\"nsvcquyxbwilsxxemf\\xd9leq\"\n\"y\\xcetxuafl\"\n\"it\"\n\"kwdlysf\\\\xjpelae\"\n\"viwh\\x58wpjjlnvryuti\\x2chngrx\\\\nhtkui\"\n\"vhn\\x9ehre\\xc3ncsqbozms\\\"nl\"\n\"ytc\\xa3mgeeogjcqavmmmd\"\n\"xzlexlitseozoxtpzzutfq\"\n\"cish\\x07lmovj\"\n\"ekbflwqzaiivdr\\\"pq\\\\azrfbntqwkn\"\n\"uc\\\"xdbegmlmhksofzohavtrnxf\"\n\"xfdnrdqdrcjzbe\"\n\"ndg\\\"ckgrpisib\\\"rg\\\"p\\\\lmpfzlssnvk\"\n\"witfjwpbyyzlop\"\n\"zonlww\\\"emrbcsgdtrg\\\"rjzy\\x64zqntlw\"\n\"dvgb\\\"zn\\\\vrbzema\\\"ckmd\"\n\"\\\\vdlmxhlvldk\\\"pmzazeip\"\n\"\\\"\\\"r\"\n\"rsntinv\"\n\"iy\"\n\"lr\\x20efh\"\n\"csgexlb\\\"zqdavlxxhtdbh\\\"\\\"\\x0fkpvhiphm\"\n\"ouwhp\\\"ogbft\"\n\"cm\\\\ckltng\\\"dw\\x8brf\\xf0eppgckd\"\n\"zmnlsgalhpkejsizfsbtnfliu\\\"nhc\"\n\"pnrkaayqvwpdjbhcrbb\\\"yfeq\\\"aq\"\n\"ozh\\\\hoxow\\x2csrtr\\\\r\\\"\"\n\"bqxabj\\\"u\\\\s\"\n\"cpsjti\\\"gy\"\n\"aa\\\"p\\\\nki\\\\ajijkqev\"\n\"q\\\"\\\"lfdentjgd\\\\\"\n\"bmokvpoebutfki\"\n\"pielvcbne\\xf6efvzxn\"\n\"kx\"\n\"zlgmqagcrbhrwtwtmmg\"\n\"aiyhmntcqjbpv\\xb5hhswxbryoedvos\"\n\"tdpaxrb\"\n\"fu\\\"\\x7dttkyvhrlwko\"\n\"oirc\\\\\\\"cqlnqffjqt\\\\k\"\n\"edxlia\\\\tcyby\"\n\"jpeybgwfayerfrfbvfog\\\"ol\"\n\"ysr\"\n\"bzwzilgwfugjk\"\n\"tlcc\\x75nukvwjgftetjcs\\xaecwc\"\n\"dsqssa\\\"vzrf\\\"sewbp\\\\ahhlmhbeihlh\"\n\"qtgmjck\\\"n\\\"guki\\\"gmdivwqxismqj\"\n\"\\\"f\"\n\"wuorvlovucngbzdszqpikyk\"\n\"dfrdsacoukmgvhbq\\\"\\\"iwto\"\n\"\\\"ey\\\"ch\\\\wcgioe\\\\\\\"ouvligmsw\"\n\"ciqlszzgs\"\n\"\\\\tzyrkaoi\\\"sopjaq\"\n\"lmtnv\"\n\"ar\\\"fqoroigiertjjlm\\\"ymgi\\\\kkjewsxd\"\n\"wehcimlvudpxtamdn\\\"rwy\"\n\"hr\\\"zvrwthr\\\"vruzqfrldn\\\"b\"\n\"sggekodkiwvym\\\"mhsco\"\n\"ltlkfbrrdvk\\\\\"\n\"uut\\\"sfjnz\\\"\\\\ef\"\n\"hxilg\\\\\"\n\"zsredsiwlzrpedibn\"\n\"vtfi\"\n\"\\\\h\"\n\"qekfrc\\xf6wduodbwrguqcng\\\\n\"\n\"\\\"lljlfdrxftwidn\\\\pkv\\xd9ij\"\n\"mrvgqynpehkliuijlpp\"\n\"gikjph\"\n\"yoxcdrdt\\\"wbaurnyhoyxoihu\"\n\"onmomwuxuammbzxe\"\n\"rnrr\\\\twviz\\x61gqaljr\\x0dmtw\"\n\"r\\\"vupaoi\"\n\"l\"\n\"sei\"\n\"jwxtdtbkd\\\\kxd\"\n\"\\x22v\\\\\"\n\"ahd\"\n\"j\\\"bjqxs\"\n\"\\\\i\\x24gglxub\\\\nzsajokt\"\n\"lviwpu\\\"uxdlh\\\\zuy\\\"xqy\\\"ytdzlx\\\"r\"\n\"kptfmys\"\n\"fwxzikfhczkjwyjszqdbkepaeellc\"\n\"nlqpsvbrbd\\\\ns\"\n\"qryuwkjiodw\\\"\\\"vaqyq\\\"dmyifm\"\n\"tw\\x15kdmaudjl\\\\zorhp\\\"alwh\"\n\"aatrvczesykekkjfyb\\\"kb\"\n\"usqcutbqbxxhucwxo\\xc1ltb\\\"j\\\"bghjcvws\"\n\"ilhsrnzxkz\"\n\"bianqfdfdhvw\"\n\"hqibqs\\x7ax\\\"qoxqoaqtcsz\"\n\"htxtoojbbauztwxuiq\\\\ngyfy\\\\obzc\"\n\"rxn\\\\moxlj\"\n\"mtus\\x84erh\\\"dbe\"\n\"asx\\x50huvsitcxadt\"\n\"\\\"bugggtnrc\\\"\\\"kl\\\"hmpu\\x83hqrvhpo\"\n\"ewisbp\\\"\\\"vuzf\\\\w\\x5fvalszdhl\"\n\"scusplpwxfnxu\\x57\\\"zynpn\\x99xerc\\\\ri\"\n\"m\\\\kinmkke\\x0cl\"\n\"xhuzit\\x7fd\"\n\"kfbo\\x04\\x50ruqirn\"\n\"t\\\"\\\"xpbdscmdoug\"\n\"punvpsgnbgyxe\\\"sptmpz\"\n\"bxukkazijr\"\n\"nxyrcdaoo\\\"rjkk\\\"wntehcvcip\\\"vrd\"\n\"rdpvqskmihqaw\"\n\"p\\\\gwdhtqnpgthod\"\n\"nwnuf\\\"\\\"yebycearom\\\"nqym\\\"\\xd4sii\\xccle\"\n\"alda\\\"ptspo\\\"wkkv\\\"zoi\\\"hkb\\\"vnntyd\"\n\"ixpgpfzbqv\"\n\"znui\\\"\\\\fzn\\x03qozabh\\\"rva\\\"pv\\x67\"\n\"e\\\"zswmwuk\"\n\"hcccygwfa\"\n\"ngmace\\\\rtyllolr\\\"\\x68bw\"\n\"\\\\c\\\"jyufbry\\\"ryo\\\"xpo\\x26ecninfeckh\\\\s\"\n\"hdnpngtuc\\\"dzbvvosn\\x31fwtpzbrt\"\n\"hesbpd\\xd4\"\n\"dsdbstuzrdfmrnyntufs\\\"dmv\"\n\"d\\xeeibcwhcvkt\"\n\"fvzwrsfjdqdmy\\\"\\\"v\"\n\"ns\\\"dqafz\\\\lkyoflnazv\\\"mn\\x37\\\"o\\\"yj\\\"e\"\n\"dypilgbwzccayxa\\\"bnmuernx\"\n\"q\\xa9ztqrhreb\\\"\\\"kxfeyodqb\"\n\"iz\\xa5qjxqulaawuwz\\\"rqmpcj\\\\yel\"\n\"z\\\"\\\\pq\\\"\\\"y\\x67zpjtn\"\n\"ifxqvivp\\\"kiiftdoe\"\n\"jxzebj\\\"\\x35\\\"qr\\\"ecglcutuoyywqumcs\\\"kk\"\n\"q\"\n\"yob\\x85qmpuwexptczbkrl\"\n\"cjiavv\\\"uudpozvibyycnmxhxpxmpjoz\"\n\"xro\\\\uiqyrcid\"\n\"nod\\\\k\"\n\"d\\\"neiec\"\n\"tqyrqvwyvmz\\\\pzgzzcqsqsrgbqbtapoz\"\n\"r\\\"xvocpeuxfxslgueb\\x05kzyyie\\\"aoec\"\n\"\\\"du\\\\uirlhcbgv\\\\cjqhfreqnvn\"\n\"zp\\x04\\x15\\\"pbjwhrjtmiba\"\n\"\\\\cv\\\"\"\n\"k\\\"rwnb\\\\hiu\\\"rqd\\\"rc\\\\nyakrhly\"\n\"klrmafjzandiddodgz\"\n\"xipzhqzhvlpykzcuppx\"\n\"zdvrvn\\xd0mtfvpylbn\\\\\\\\sxcznrzugwznl\"\n\"ody\\\\pvm\\\"kpjiudzhxazirgxzvumeat\\\"o\"\n\"kllvhdp\\\"prjikzrrc\\\"adgpegc\\\\\\\"gk\"\n\"sqtpug\\xbcaauxaamw\"\n\"wegxxrrxdvpivrqievfeokmnojsk\"\n\"\\\\bo\"\n\"gijhz\"\n\"ylowluvabwrigssdgtxdwsiorxev\\xdd\"\n\"\\\"\"\n\"ghnsrnsqtxpygikahkrl\"\n\"\\\"rcfqkbjf\\\"sgxg\\\"vnd\\\\rotn\"\n\"ap\\\"smgsuexjrbuqs\\\"mpbstogj\\\"x\"\n\"koaunz\\\\sgt\\\"opv\"\n\"yialiuzwix\"\n\"yp\\\"ndxgwzml\\\"bt\"\n\"lpcjxmggfsy\\\\szbxccarjkqzasqkb\\xcfd\\x0c\"\n\"x\"\n\"mgakc\"\n\"vjieunoh\\x73fjwx\"\n\"erbvv\\\"qulsd\"\n\"mimycrbfhqkarmz\"\n\"tihfbgcszuej\\\"c\\xfbvoqskkhbgpaddioo\"\n\"mziavkwrmekriqghw\"\n\"izk\\\\tnjd\\\\ed\\\\emokvjoc\"\n\"c\\\"nhbqzndro\\\\g\"\n\"usfngdo\"\n\"aypljdftvptt\"\n\"ym\\\"afvq\\xbcc\"\n\"zabi\\\"wjpvugwhl\"\n\"ebvptcjqjhc\\\"n\\\"p\\\"dxrphegr\\\\\"\n\"mzlqqxokhye\\xd9\\\\rffhnzs\"\n\"hnipqknwpsjakanuewe\"\n\"rqgbfcjdrmiz\\\"h\"\n\"kzzp\\\\z\\\\txmkwaouxictybwx\"\n\"yzmspjkqrteiydswlvb\"\n\"gjpxklgpzv\\\"txri\\\\hotpuiukzzzd\"\n\"p\\\"rxergtbsxmjmkeeqwvoagnki\\\"\"\n\"santipvuiq\"\n\"\\\"ihjqlhtwbuy\\\"hdkiv\\\"mtiqacnf\\\\\"\n\"oliaggtqyyx\"\n\"fwwnpmbb\"\n\"yrtdrieazfxyyneo\"\n\"nywbv\\\\\"\n\"twc\\\\ehfqxhgomgrgwpxyzmnkioj\"\n\"qludrkkvljljd\\\\xvdeum\\x4e\"")
(def input (split raw-input #"\n"))

(s/def ::quote #{\"})
(s/def ::backslash #{\\})
(s/def ::hex-escaped-char (s/cat :backslash ::backslash
                                 :x #{\x}
                                 :char-1 char?
                                 :char-2 char?))

(s/def ::item (s/alt :hex-escaped-char ::hex-escaped-char
                     :backslash ::backslash
                     :quote ::quote
                     :plain-char char?))
(s/def ::list-line (s/* (s/cat :item ::item)))

(defn hex-escaped-char->char [hex-escaped-char]
  (-> (str "0x" (hex-escaped-char :char-1) (hex-escaped-char :char-2))
      edn/read-string
      char))

(defn process-list-line
  [list-line]
  (let [handle-line-item (fn [item]
                           (let [[item-type item-contents] (first (vals item))]
                             (if (= item-type :hex-escaped-char)
                               (hex-escaped-char->char item-contents)
                               item-contents)))]
    (apply str (map handle-line-item list-line))))

(s/fdef process-list-line
  :args (s/cat :list-line ::list-line))

(defn in-memory-length [list-line]
  (- (count list-line)
     2))

(defn code-length [list-line]
  (- (apply +
            (map (fn [item]
                   (let [[item-type _] (first (vals item))]
                     (case item-type
                       :plain-char 1
                       :quote 2
                       :backslash 2
                       :hex-escaped-char 4)))
                 list-line))
     2))

(comment
  (s/exercise ::hex-escaped-char 10)


  (->> "\"\\x27\""
       vec
       (s/conform ::list-line)
       code-length
       )


  (hex-escaped-char->char
    {:backslash \\, :x \x, :char-1 \8, :char-2 \b}
    )

  (s/conform ::list-line (vec (first input)))

  (process-list-line (s/conform ::list-line (vec (first input))))

  (first input)

  (val (ffirst
         (s/conform ::list-line (vec "\"\\x27\""))))

  (s/conform ::list-line (vec (first input)))


  (identity 0x27)
  (char 0x27)

  )
