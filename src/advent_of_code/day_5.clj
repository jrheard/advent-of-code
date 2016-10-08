(ns advent-of-code.day-5
  (:require [clojure.spec :as s]
            [clojure.spec.test :as stest]
            [clojure.string :refer [split]]))

(def raw-input "rthkunfaakmwmush\nqxlnvjguikqcyfzt\nsleaoasjspnjctqt\nlactpmehuhmzwfjl\nbvggvrdgjcspkkyj\nnwaceixfiasuzyoz\nhsapdhrxlqoiumqw\nlsitcmhlehasgejo\nhksifrqlsiqkzyex\ndfwuxtexmnvjyxqc\niawwfwylyrcbxwak\nmamtkmvvaeeifnve\nqiqtuihvsaeebjkd\nskerkykytazvbupg\nkgnxaylpgbdzedoo\nplzkdktirhmumcuf\npexcckdvsrahvbop\njpocepxixeqjpigq\nvnsvxizubavwrhtc\nlqveclebkwnajppk\nikbzllevuwxscogb\nxvfmkozbxzfuezjt\nukeazxczeejwoxli\ntvtnlwcmhuezwney\nhoamfvwwcarfuqro\nwkvnmvqllphnsbnf\nkiggbamoppmfhmlf\nughbudqakuskbiik\navccmveveqwhnjdx\nllhqxueawluwmygt\nmgkgxnkunzbvakiz\nfwjbwmfxhkzmwtsq\nkzmtudrtznhutukg\ngtvnosbfetqiftmf\naoifrnnzufvhcwuy\ncldmefgeuwlbxpof\nxdqfinwotmffynqz\npajfvqhtlbhmyxai\njkacnevnrxpgxqal\nesxqayxzvortsqgz\nglfoarwvkzgybqlz\nxdjcnevwhdfsnmma\njyjktscromovdchb\npvguwmhdvfxvapmz\niheglsjvxmkzgdbu\nlwjioxdbyhqnwekv\nzcoguugygkwizryj\nogvnripxxfeqpxdh\nhkvajhsbfnzsygbm\ncnjqeykecopwabpq\nwojjtbcjinoiuhsj\nkpwpvgxbyzczdzjq\nwrvhylisemlewgzk\nuiezkmnhilfzahtm\nmucteynnuxpxzmvt\nzaiwbgxefusfhmst\napptbogpxivjwink\nqryboarjtwjhjgjb\nirehxupgyseaahzd\nfobstqxguyubggoh\nysriumfghtxtfxwe\nauchdmasvfeliptw\nmztuhefcrnknyrdl\ntyjmkhihbwabjtaa\nyquzkdtgsljkaebw\nalmvdvofjtkyzbmd\nemqftiuqqpdwwbrv\nhrrhmqfpepvbawvw\natrkgykycvgxbpyb\ndhthetnealksbdan\nzzqafhgicubptiyo\nqdtaieaziwhbttnw\nkyskgapdgqrtrefw\nedwzlpqztpydmdlr\nawszjnlmvlyqsuvl\nkcrtmtshtsgixvcp\njtaskgkijivbbkri\nmmggfwapsetemiuj\nitagrrnjbnmhgppd\nuqmbezechbrpbnqq\nnnyimvtascflpzsa\nknqeimypkdttyudj\nvgoiyvtvegwyxjjd\nqubzdxsbecktzrho\nzehojtvktsbbxijb\nxepmjrekwcgoxyoh\nbnptxnocbpbqbyeq\nsfvynsywscbnymos\ndsltfbpcmffbluba\nkncrlzlmkikylppa\nsiwudrvmildgaozv\njhhefbvbvneqzvtc\nlqjgztxitbuccqbp\nhimmwlbhjqednltt\nvwognchyertnnfil\neejakhapkbodrntf\nqxuijkkhhlskgrba\naankpfxxicfpllog\nvuxykvljyqexfhrn\nepgygflbxlbwybzq\nzuxmwvetmvcszayc\nxttwhfqmemgtjnkf\nhftwldmivyfunfvl\nbejlyxfamzliilrj\nzkehazcxyyvtrxti\ndsgafehmcfpycvgz\nigremmqdojqdvwmb\nswnjzvmhcslvkmiw\nfchzbfbmtqtxmaef\nxwjmyyrlznxrcytq\nbrwcwzpcvbwdrthl\nfvrlridacsiojdmb\nmhsturxdlmtxozvy\nusxvqyrwywdyvjvz\ngwazuslvmarfpnzm\nrgkbudaqsnolbcqo\ndpxvlbtavdhdedkj\nnnqmjzejhodyfgyd\nozoazxkfhujgtzvy\npsdgvhzdiwnuaxpl\ntznkilxpogbzgijz\nwnpytcseirtborhh\nlhauurlfsmagfges\noqfbzixnlywkzwwy\nyoehapoyjpakziom\nvtjftdcsfdzbmtrn\nzcshfnodiwixcwqj\nwapbxpaxgjvtntkm\nqfyypkyvblrtaenh\nbsxhbxkovgukhcza\nkitdmvpiwzdonoyy\nslkbhxmehzavbdsf\ndovzjouqkzkcmbkl\nqpbigdcqkfnfkxvq\neaiaquhnesvtcdsv\nmhbezlhqojdsuryj\ndqprkkzxlghkoccx\nxqepmorryeivhrhm\nfrwmrjpezwmjflvf\ngjpfgwghodfslwlf\nfzyvajisdjbhfthq\npvzxkxdscdbilrdb\nmtaxmqcnagmplvnm\nrlyafujuuydrqwnc\ngvqvrcxwyohufehq\nlmrkircgfrfusmfd\novlpnkxcpimyaspb\nxhyjremmqhdqywju\npxfczlhpzbypfarm\nutjhprzhtggausyp\nutzkkzlnyskjtlqh\ncecbcnxpazvkedic\nxwvoaggihrbhmijq\nkrredhmtwlfmyagw\nlwfhxgbknhwudkzw\nvyczyvuxzmhxmdmn\nswcoaosyieqekwxx\nwaohmlfdftjphpqw\ngaclbbfqtiqasijg\nybcyaxhluxmiiagp\nxgtxadsytgaznndw\nwzqhtjqpaihyxksm\nfdwltsowtcsmsyhm\nrpoelfbsararhfja\ntswgdacgnlhzwcvz\nxjgbhdlxllgeigor\nksgthvrewhesuvke\nwhgooqirdjwsfhgi\ntoztqrxzavxmjewp\nhbkayxxahipxnrtl\nlazimkmdnhrtflcu\nndoudnupbotwqgmr\nniwuwyhnudxmnnlk\nhlmihzlrpnrtwekr\nwzkttdudlgbvhqnc\nrfyzzgytifkqlxjx\nskddrtwxcyvhmjtb\nmljspkvjxbuyhari\nxwkhozaoancnwaud\nnookruxkdffeymdz\noiqfvpxmcplyfgoa\nqoxggshmrjlzarex\nlsroezewzkrwdchx\nnkoonmvdydgzspcl\nlygxeqztdqklabov\njempjyzupwboieye\nhpdaqkhjiddzybly\ncvcizjlnzdjfjlbh\nvaaddsbkcgdjhbkj\npjxmtxoyrkmpnenf\nujqdvyqnkbusxlps\nmiyvzkzqploqaceb\ngapcsbkulicvlnmo\nxqpcyriqhjhaeqlj\nipumdjwlldzqhmgh\nswdstecnzttmehxe\nucmqordmzgioclle\naywgqhmqlrzcxmqx\nptkgyitqanvjocjn\nwcesxtmzbzqedgfl\nrnetcouciqdesloe\nchpnkwfdjikqxwms\nonpyrjowcuzdtzfg\ntydnqwaqwkskcycz\ndhamguhmkjzzeduy\noecllwyrlvsyeeuf\ngsukajpoewxhqzft\nsgdnffdixtxidkih\npqqzjxzydcvwwkmw\nwnjltltufkgnrtgm\nhylaicyfrqwolnaq\novfnugjjwyfjunkm\nxknyzsebmqodvhcl\nuwfmrjzjvvzoaraw\nzaldjvlcnqbessds\nzphvjuctrsksouvz\nceqbneqjwyshgyge\nwmelhaoylbyxcson\nnghuescieaujhgkj\ndhjmflwwnskrdpph\nexvanqpoofjgiubf\naidkmnongrzjhsvn\nmdbtkyjzpthewycc\nizctbwnzorqwcqwz\nhrvludvulaopcbrv\nmrsjyjmjmbxyqbnz\nsjdqrffsybmijezd\ngeozfiuqmentvlci\nduzieldieeomrmcg\nehkbsecgugsulotm\ncymnfvxkxeatztuq\nbacrjsgrnbtmtmdl\nkbarcowlijtzvhfb\nuwietqeuupewbjav\nypenynjeuhpshdxw\nfwwqvpgzquczqgso\nwjegagwkzhmxqmdi\nvocvrudgxdljwhcz\nnnytqwspstuwiqep\naxapfrlcanzgkpjs\nlklrjiszochmmepj\ngxadfpwiovjzsnpi\nqidsjxzgwoqdrfie\nwgszciclvsdxxoej\nkwewlmzxruoojlaq\nywhahockhioribnz\nucbqdveieawzucef\nmdyyzmfoaxmzddfv\nhsxnabxyqfzceijv\nvivruyvbrtaqeebr\njxfeweptjtgvmcjc\nmmypqxmpurhculwd\nmpiaphksvctnryli\nxqzqnuxmuzylkkun\nfndmtefjxxcygtji\ndnorqlldvzqprird\nnutokyajmjpwjaqu\nvlupfperqyqkjcaj\ndgihjeokrphkpdnk\nnvbdyrlheqzixuku\nmhrkntnxvsmvrpka\nkvhkyanlhhymwljf\nfhipumtegqfgeqqw\nvpfjgveycdefuabu\nkzincljffncylcsf\ntsezxymwmjtyegqw\nwxhcdrqedkdcwxli\nueihvxviirnooomi\nkfelyctfvwyovlyh\nhorzapuapgtvzizz\niiqkdpmfvhwwzmtj\nrsaclclupiicstff\nquwkkhrafypkaoum\ngyrgkgmwqfkeudfe\nnoydhbqacwptyfmy\nefwwuipzgtkwffhf\nsuyojcitomdxsduh\nlbcxnsykojkufkml\nzpglsvoutvzkgdep\nusgrufyvgsbsmbpr\nkatrrwuhwvunjqor\nbtngwrpcxoyfbgbc\nbxjscjdiowjrkpns\nnwxvnfrnlkgqxvhf\nikhyqkvljucgdlag\nxibnxsjopmxvflkl\nmzplumcfivqcjqnz\njqflcxoxzlbwlxry\nfcscvmfepdxrshxe\nwlpffwunffklzbuc\nemvrlqajjgwzfmle\nrhaheurtzrfoqkyq\nifuuhpxmadaysfsx\nncyfvleyzqntpcoo\nzeogmyaqccmtvokd\njqppbzebppdnpurn\nxixarswxsiwjzgni\nezruwzajsoombphs\nhmiqfeizyprielxf\njnaoxljnftymsfey\nextgzrxzovlsixnf\nyhyfmovvlrwoezsv\nffnybaolppuzpjym\npqowimdiusccaagn\njgceiosiihpjsmnu\nhkoexeaopebktngx\nnjhzuvsygymejqav\nyjkgcclgtvushcfk\ngmbjxhnkkxlihups\npdlwysadiebsidjz\nomrwmgzulfoaqros\nofvvgdezwvcffdcy\notytpuklhxcpxhgd\neyfaosxdauumvlux\nmvdthjfstrlqlyuo\nmdgdchgnlxaxspdm\nbakjezmhbwqxzevd\nmsakswaphdwaodhg\nvjcqscgdbnsxdllh\njjywaovewbuzreoj\nnqvplhwacylifvwk\nlpwmpixbxysmsign\nflcvbpxrchcpbgcb\nqjpkeuenenwawlok\nbnqkflfmdmntctya\nfzsgzpoqixvpsneq\nicwfdisutoilejld\nrelchofohnkwbumi\naljalgdaqwhzhfwr\ncahkvnwnbwhodpqs\ndnrzeunxiattlvdm\nnsmkhlrpwlunppjs\nmqqsexlwfqnogwub\ntfavelkqrtndpait\nooguafrnmprfxcnz\nntynkiordzxtwrqa\nrkkyzlxekqqlkvym\nofxcivdnwcmgfnme\nywotqwbrqxlrnobh\nnrbbiypwhrqihvev\nflqsjixxtydheufs\nlcfrfzypstrqctja\nhyzbuzawuzjrynny\nexfbywcnstebnvmq\nvydzwnbmcihvqrnj\nqmwqaaylinzrdmiw\nlpxpztpvfggspeun\nlhxmqqbracsuyrfm\nzgkwsrabaseidbrw\nyjlmbhbqsqgszsun\nmqfzqtbxtuteabtd\nizomzdmcqmfrevwd\niqijrlqurdwrkoln\nfxhqzpgoxxjkkhql\noulwontmgrjeopnk\nedaigfydjexvzzvj\nvjhybiklxpxjqpwc\nypxfbfnpbmqmwtte\nxzvcsgasztrxdzud\nrpulqmobptfarboo\npalacmdijxzzykrf\njmllwukplufohiby\ndnswayomusiekfmy\nsxbrjqtqgzzwhcfo\nlylvndsgbnbqiejm\njaxxhoulxnxnaenr\nnblissutfazbcpwn\nzmlsjszzldvbiacr\nkewojtlchfkclqwk\neqvfjasddggvfame\nyibzqlvxtraxpdon\ndgnbxsbmdrtyvaac\nuoxrcxfimhgtxqhy\nxfdxalrwcwudlviq\nxmtbdklqptoswpwl\nzezyopzdztdjerfl\nxuzluhjsqvhytgbc\nqdjtmeckispmgzki\nphakupesplzmmmvc\ngpuoqfffumzszybn\nbhywxqkrrlwuebbw\nibvwgoyvelzenkzl\nncohvvbmiekbaksa\nfzuvqzvxvdbeirrp\nlshtzniokucwojjd\npunrduvlnrulkium\ngnfpikidnfobrrme\nvxkvweekmnvkzgyl\nrhydssudkcjlqgxn\ncjtqvlaahohcgumo\njwzmfyinsfwecgcb\nblpeseqhlzfilpuf\njvtpjkyokzcvagon\nqjomincbcobjczpe\nugsyzkzgdhxtmsfz\nhleaqgwzqjwajcra\ncoumfghptpnxvvov\nhqpnbupnzwpdvgqd\ncpouyodqxgviasem\nlljvxeyozckifhfd\nhuqtnvutdyfgwtwa\nyenlveuynmlmmymu\nojdyufkomxiwjmbf\nspjzgvcwvzgffjkk\nvxykmjhyvmhyssbp\ntazdeqggfcjfvwwn\nuumwcngwcytvpufx\navovuzkrevloneop\nowczrtbnrvjfemkt\nhzpugcanaxyvaokj\niishlodnxvjtgzyn\nqosdonclrnxirham\neonqlnwevahydddg\nryqmnuikftlxuoqy\nwhqepbcwabzbthha\nvekisvnwhgpyemxr\nlrwxzoamnvpnlhap\nywepvqthnorfswjv\nevqwvsoazmwyypjy\nbgwoojddubppmjxf\njypkfrthzgtyeddi\ntynabbhfjzkrqsju\nadxstbfqheuqbcuk\ngqwqiocdyqoiblrx\nybuddlyuskdlegxv\nluwynbsmpgyeqsbr\nltyqgqoyljibqndo\njaedpajzphfybajh\nepglnrxofptsqvmy\nzjdpxkngfkstxbxh\nekegphcwanoickfu\ncqvhuucvejqirvfs\nuqudnnqumsqcgefo\nqnzunermlnpcfflo\novyxaniqaawzfuxx\ndjekxcezjowdhopq\nbwtwbmdehrhpjnlk\nnilsnlacerweikfa\nhyrigsrmsrzcyaus\ngvmdmgddduylmxic\newzovdblhmjgjwsk\nojjfsknlonzguzlq\nyjgfruvpjvlvrvvq\ncyoryodwyhzwprbv\ncrsjclrurcquqgut\nsjhfhobwtojxcmem\nibxfjudilmdeksea\nuqbhdbjoeupyhbcz\nuqbxigzxuxgmjgnw\njashafmtzrhswirg\ndexiolovaucyooka\nczjbwwnlwcoqnoiu\nojigosazigfhttjc\nzfiqtgrqbmftknzn\ndlzbmvmolssbqlzl\nsgmchcurrutdtsmw\nscdwjqsdohcdrwry\ncgtdvecqwplpprxn\niiplenflfczaktwi\nwmgnwfxfcjhyeiqg\ngiihshowtcatecvl\nnqhzfincclumvkaz\nkxstpzgdfvepionc\nagbhxcijxjxerxyi\nhmgfqevgdyvisyvs\ntthakmvpowpvhtao\nottalcghygpaafbo\naplvozayycremgqg\ndbjxlnaouxqtdpfz\npeeyallzjsdvpalc\nndtdjyboixuyhfox\nllabnbcobexfoldn\ncweuvfnfyumbjvxr\newkhhepaosalnvkk\npivyiwsiqpwhagyx\nauzsnwdcerfttawt\ngrbfrekupciuzkrt\nbyfwzadtzrbndluf\nlluypxjeljzquptk\npskwsnhqanemtfou\nsxvrtqqjdjkfhhrm\nulsmqgmshvijyeqh\nqigofesfhekoftkf\nzhatniakqtqcxyqa\nuuczvylgnxkenqee\nmlitvtuxknihmisc\nsrrtrxdvcokpyfmz\nosispuucklxcfkeb\nvqhazlaulmnpipql\numkiueljberqhdig\nknvpbkbvgoqzwprp\nnbsocqikhuvsbloj\nwjnpepjkzkednqbm\nagbhmytsofuyqcor\ngvogzhkkpxyfecko\nardafguxifeipxcn\nyiajcskbgykyzzkw\nsejunbydztyibnpq\ndqrgfggwcnxeiygy\nxnqqwilzfbhcweel\njjtifhlvmyfxajqi\ngwszrpgpmbpiwhek\nkydzftzgcidiohfd\nefprvslgkhboujic\nkecjdfwqimkzuynx\nrildnxnexlvrvxts\ndlnhjbqjrzpfgjlk\nqluoxmzyhkbyvhub\ncrydevvrjfmsypbi\ndosaftwumofnjvix\npwsqxrfwigeffvef\nnzyfmnpwqyygjvfx\niccbckrkxlwjsjat\nbmputypderxzrwab\nbhuakynbwnlreixb\nqmrzfyqjiwaawvvk\njuvtixbkwyludftn\nzapmjxmuvhuqlfol\npaiwrqjhpjavuivm\ntsepfbiqhhkbyriz\njpprewufiogxoygk\nmmapyxbsugcsngef\npduhmgnepnpsshnh\naetndoqjvqyjrwut\nfnfvlorhwpkkemhz\ngedfidpwvoeazztl\nbeclvhospgtowaue\nwsclsvthxustmczm\ntjbxhnpniuikijhe\nrhetyhvfcemponeg\nmavonujurprbeexi\nargbrpomztrdyasa\nbzvtffbtygjxmkvh\nmaqyqkhsqgzfzvve\nseeirbiynilkhfcr\nwxmanwnozfrlxhwr\ndieulypsobhuvswb\nnxevassztkpnvxtb\njclxuynjsrezvlcy\nxlolzyvgmwjsbmyf\ntguzoeybelluxwxc\nfkchoysvdoaasykz\ncyynwbfcqpqapldf\nrhifmzpddjykktuy\nndvufsyusbxcsotm\ntxutnzvdsorrixgg\nqjoczhukbliojneu\nufhwujotncovjjsz\nkclsgsdwcrxsycbr\nyscwmlrdaueniiic\nnxhivrovpkgsmugb\nfdxqfyvwwvgeuqkv\nfemtamfylysohmpr\namsyzslvyxsoribh\nnhmqxncwsonhgbcz\nuomqsvcbpthlmcue\nkxtfapcqrnjkkslj\nxtieihonlfubeync\nadpcjqxgydulchgj\ncjynnzsmmujsxxpd\nneeapmzweidordog\nszoivgqyqwnyjsnk\nuwgrtzaqezgphdcu\nptpgttqxocjwxohi\nfhltebsizfwzpgpf\nemmsazsidspkhgnh\ndxcprkbcjeqxqzgn\ntpxzqwxbzwigdtlt\nafsmksnmzustfqyt\nxyehnftstacyfpit\nvcrfqumhjcmnurlw\nrrznpjzcjgnugoch\ngbxnzkwsjmepvgzk\njwobshgwerborffm\nzmuvfkhohoznmifs\nbuyuwgynbtujtura\nbevncenmpxfyzwtf\nhqqtcrhzfsrcutjh\nkbpzshllpiowepgc\nalspewedcukgtvso\nxvsvzzdcgjuvutrw\npmwulqraatlbuski\nabuzsiinbueowpqn\noedruzahyfuchijk\navhcuhqqjuqkesoq\nazqgplkzsawkvnhb\nrjyoydogkzohhcvx\naezxwucqvqxuqotb\nkxobnsjvzvenyhbu\nnnjoiilshoavzwly\naijttlxjrqwaewgk\ncvsaujkqfoixarsw\nzngtoacpxcsplgal\nqhkxliqtokvepcdv\naixihrtdmxkfvcqw\nowbgdgdymxhhnoum\ntajsagmruwzuakkd\nckrfduwmsodeuebj\nalfdhuijuwyufnne\nxpchlkijwuftgmnm\nrwcrvgphistiihlg\nxdaksnorrnkihreq\nakeschycpnyyuiug\nrgputhzsvngfuovz\nlerknhznuxzdhvre\nmqiqmyladulbkzve\ncsnmupielbbpyops\nkwgrwgmhfzjbwxxz\nnpwtvbslvlxvtjsd\nzxleuskblzjfmxgf\nhexvporkmherrtrn\nrhtdhcagicfndmbm\nqhnzyuswqwoobuzz\ndpvanjuofrbueoza\nkjcqujmnhkjdmrrf\ngholddsspmxtpybg\njihlvyqdyzkshfsi\nzuviqmuqqfmtneur\nkzexjowatvkohrtx\nwgijnfhibsiruvnl\nzevkrkmhsxmicijb\nkhxrcteqourjvoxa\nylpxlkcnenbxxtta\nzrfsvctbojjkpvtw\nnlzbudxibnmcrxbt\ncqnscphbicqmyrex\nywvdohheukipshcw\nriwatbvjqstubssf\nidlztqqaxzjiyllu\nsdpdgzemlqtizgxn\nrjtbovqlgcgojyjx\nfnfrfwujmjwdrbdr\nosnppzzmrpxmdhtj\nljhwngclvydkwyoe\nchwqkrkzrvjwarat\njmydkwpibkvmqlgs\nzvhfmbxnlxtujpcz\njsnhsphowlqupqwj\nfzhkkbpasthopdev\njerntjdsspdstyhf\ngctwmaywbyrzwdxz\nxemeaiuzlctijykr\nxulrqevtbhplmgxc\nyfejfizzsycecqpu\ngboxrvvxyzcowtzm\nlpvhcxtchwvpgaxp\nwdiwucbdyxwnjdqf\nqgwoqazzjlvnjrwj\nprtlnkakjfqcjngn\nfagvxsvjpuvqxniz\nxacmxveueaakfbsm\nginvtonnfbnugkpz\nqpvggsppewfzvwin\nreoqnlzruyyfraxa\nkolwtqhifjbbuzor\nvrkcywvdhdprztww\nngdvyfmvjqhbzbxt\nrooxeoilqzqjunmp\nefxmdprtogtxgyqs\nqrhjuqndgurcmwgu\nouitjprueefafzpl\nkirdwcksqrbwbchp\nfpumsmogojuywezo\nlgjrgykywugzjees\nxigioqcpjabpbdas\newkhuprpqzikmeop\nfgrgxsqeducigxvr\nbclkursnqkzmjihl\njozidniwvnqhvsbc\noghcilcyozrmmpta\nxbgmaungzcpasapi\niqowypfiayzbcvhv\nopdehgwdgkocrgkf\nzfzvdjeinlegcjba\nvhakxvlcayuzukap\nxyradgyiebpevnwe\neamhtflgedwyshkn\nigteqdgchjeulfth\nkwsfkigxzpbgdxod\nvapnpsbdboiewpzp\nwbuqhjsngxpqshen\nvxxilouxuytitwgm\ncpnwlkwnkeanqnet\nwdmbtqvvlowftvgb\nwjtmcecpyqzwpbqg\njnxmoxdhvsphcdeg\nwabxfxpotoywwodn\nmwbsoxzlqpqobvvh\ncoktshbyzjkxnwlt\nrzhnggpslwzvyqrp\ndgzuqbzarbutlkfx\nwunajaiiwgijfvjh\nuotdbcgmsvbsfqlb\nkxdtlgmqbccjqldb\nngmjzjwvwbegehfr\ncvpsabqfpyygwncs\nwqluvqlhdhskgmzj\nrbveperybfntcfxs\nfbmoypqdyyvqyknz\nzxpgzwnvmuvkbgov\nyexcyzhyrpluxfbj\nltqaihhstpzgyiou\nmunhsdsfkjebdicd\nplecvjctydfbanep\nkjrxnnlqrpcieuwx\nzbcdtcqakhobuscf\nkgovoohchranhmsh\nllxufffkyvuxcmfx\ntgaswqyzqopfvxtw\nkojcqjkdpzvbtjtv\nxggdlkmkrsygzcfk\nvvitpsnjtdqwyzhh\ngcqjuwytlhxsecci\nvbsghygcsokphnrg\nvejqximdopiztjjm\nhudqtwmwkviiuslp\nvwswfvpcwwpxlyry\ngxmfiehdxptweweq\nqjmekjdcedfasopf\npqyxdxtryfnihphf\nfelnavctjjojdlgp\nhbimufguekgdxdac\ndhxhtnqgfczywxlr\npssottpdjxkejjrh\nedieanguabapxyig\nsciinanyqblrbzbb\nirxpsorkpcpahiqi\nqsxecaykkmtfisei\nivfwlvxlbnrzixff\nhqxzzfulfxpmivcw\nvvbpaepmhmvqykdg\ncetgicjasozykgje\nwuetifzdarhwmhji\ngaozwhpoickokgby\neldnodziomvdfbuv\nfavpaqktqaqgixtv\ntwbcobsayaecyxvu\nlzyzjihydpfjgqev\nwnurwckqgufskuoh\nfxogtycnnmcbgvqz\naetositiahrhzidz\ndyklsmlyvgcmtswr\nykaxtdkjqevtttbx\nkfmnceyxyhiczzjm\nnnizopcndipffpko\nyjmznhzyfinpmvkb\nsljegcvvbnjhhwdd\nzmkeadxlwhfahpwg\nrwvcogvegcohcrmx\naguqwrfymwbpscau\nvlusytjagzvsnbwe\nsmvzhburcgvqtklh\nrfuprvjkhazrcxpv\nmegqlnoqmymcrclc\ngvldhkewtmlwqvqv\nawynhvtyziemnjoa\nvoprnvtnzspfvpeh\ndhlguqwmunbbekih\ngoayirdhnjrfuiqi\neoghydfykxdslohz\nchpippjykogxpbxq\nhqbycjweqczwjwgf\npvefsrvwumrlvhmt\neghwdovaynmctktk\ncrwkxoucibumzawc\nbzbtahvhkdigvvtj\nbnbptgihhfubxhho\nddqmbwyfmfnjjaro\ngvtswqyzazihctif\nvmqctjpgadxztqqb\ndgnndowtpeooaqqf\nsxdvctfdtalufxty\nylgeexosibsmmckw\nsxplpyskbpqnojvw\ncoarhxtsvrontyeg\nfyoaurggjupvzvlv\njlyrkqsiwuggvjem\nuwbsjoxonreuucyi\ngihuqvwxovbgokes\ndxzaaxupbcgnxcwf\ngidrgmvyrlqqslve\ncsflmlvqmonoywpx\njkxkpixlythlacnk\nejkarcdkdslldugv\ndbzmsusevohhjkmr\ncbrqzualjpdtworc\nkpgidqlmcbpfmmwu\nzwghjuofexfowqam\nncdlxmcrsmsocetz\nkfprzqacefifjkbd\nswwzivrxulkhvldc\nwgqejhigbjwunscp\nrsstnwcyybfauqxu\nqhngfxyhdqopyfgk\nzrndpyyejsmqsiaj\nxxknxwpvafxiwwjc\nmmaahwgoiwbxloem\ntabacndyodmpuovp\nyriwomauudscvdce\nduvyscvfidmtcugl\nmgipxnqlfpjdilge\nimeeqcdetjuhfjnw\ndvkutrdofpulqkyh\njefvtlktxegpmbya\niyzudqgpvlzjfydh\ngiohapxnpaqayryd\nqheqdprmnqlpztls\nrdxhijmzegxkotoq\nhdnmaspumdwnrcdz\nwafpbgehbuzdgsnc\ntbtrfztsferdmhsy\nvusndcyjngtkrtmk\nilqblestzxebcifh\nurfgjbjgzlrfsdlv\naptcdvpsqwleqttn\nbigczjvzokvfofiw\nzjnjeufonyqgkbpx\ntrcdebioegfqrrdi\njrdvdriujlmbqewt\njqrcmuxpwurdhaue\nyjlermsgruublkly\nzwarvgszuqeesuwq\nxthhhqzwvqiyctvs\nmzwwaxnbdxhajyyv\nnclsozlqrjvqifyi\ngcnyqmhezcqvksqw\ndeuakiskeuwdfxwp\ntclkbhqqcydlgrrl\nqbpndlfjayowkcrx\napjhkutpoiegnxfx\noaupiimsplsvcsie\nsdmxrufyhztxzgmt\nukfoinnlbqrgzdeh\nazosvwtcipqzckns\nmydyeqsimocdikzn\nitfmfjrclmglcrkc\nswknpgysfscdrnop\nshyyuvvldmqheuiv\ntljrjohwhhekyhle\ndayinwzuvzimvzjw\nqgylixuuervyylur\nklqqaiemurawmaaz\nhdmzgtxxjabplxvf\nxiivzelzdjjtkhnj\nktgplkzblgxwrnvo\ngvbpyofzodnknytd\nlqhlmnmhakqeffqw\nltzdbngrcxwuxecy\nobxnfjeebvovjcjz\nzexpwallpocrxpvp\ntjpkkmcqbbkxaiak\nqiedfixxgvciblih\nqcxkhghosuslbyih\ngnsfidwhzaxjufgm\nxrghwgvyjakkzidw\ntftftwedtecglavz\nwquqczzkzqrlfngr\ntwibtkijpvzbsfro\nbmplypdsvzuhrjxp\nzanrfmestvqpwbuh\nzonrhfqowyimcukm\nkpvajjfmqpbhrjma\nkujzluicngigjbtp\niusguantsrwxdjal\nkwxeuylcnszswahw\nvisdhnkobxnemldu\nrogeadmmaicwtabl\npxqycifbgevqudvs\nosaiozyvlyddylqr\nvffjxrolrpuxcatx\njbmsetccdrywssjd\nqgxyhjfpbfifmvgc\nnpejgalglldxjdhs\nmbbtqgmttastrlck\nwhapaqwdtpkropek\ndulbdboxazfyjgkg\nxaymnudlozbykgow\nlebvqmxeaymkkfoy\nbmicnfuubkregouj\ndieatyxxxlvhneoj\nyglaapcsnsbuvrva\nbbpjaslqpzqcwkpk\nxehuznbayagrbhnd\nikqmeovaurmqfuvr\nylyokwuzxltvxmgv\nhqtfinrkllhqtoiz\npjmhtigznoaejifx\nfqdbmowkjtmvvrmx\nuvqtqfoulvzozfxv\nrpajajukuxtchrjd\nsznucejifktvxdre\nufvibsmoushmjbne\nxirdqoshngthfvax\niafpkddchsgdqmzl\nvmualmlduipvykzh\nfnmuahmblwyceejb\nilsaapnswfoymiov\nlenvylifraahaclv\ncukqxlipuyxedqfh\nzgwecslpniqvtvuz\ncdcdfpsxuyrhsmag\ndszjinhantnxgqra\nioimwotsgnjeacgt\ndqcymnvjystbynhp\nyibaudyfefbfgunx\ncabslcvunjavqkbf\ngoymzvmgkvlsmugf\nzxteiitpthzskjjx\nagnxcnaqhjhlurzs\ncvmgyxhhnykuxbmb\ncgqmjexydmvgwxpp\nsygjajofieojiuna\nclpvxbrbjvqfbzvu\ncbntswqynsdqnhyv\nbztpbtwbefiotkfa\npnxccbgajvhyeybu\nasyzrvgzumtuissa\nfacjyblvcqqginxa\nrvwnucnbsvberxuv\nghrbeykzrxclasie\nekujtselepgjtaql\nkrtrzsmduhsifyiw\nticjswvsnyrwhpnt\nclmjhsftkfjzwyke\nlbxlcixxcztddlam\nxhfeekmxgbloguri\nazxqwlucwhahtvep\nkitdjrwmockhksow\nkeznwwcusgbtvfrs\nljvzxoywcofgwajj\nvebjnhnkcfzbhrcw\neqfcxkavstxcuels\nldattkyawjrvcido\nbsqqeilshcwtqyil\nfoqqsxahfiozcqrw\nliswfmuhzfbyzjhf\nsulbdcyzmolapfbs\nzuggzkelwxjpsgxb\nbetioxrgtnhpivcw\nxmtbixstdipibhgs\nttvurgqmulryyaji\nviobnljznzppfmxw\nqlzabfopydtxrlet\ntusvydegfxhaxolk\nthoufvvfjferxhwp\ncfyyzppfarjiilbs\njwmhxtgafkkgseqs\npqwuuaxbeklodwpt\nvndyveahdiwgkjyx\nssrjgasfhdouwyoh\nthbavfcisgvvyekf\nyjdvxmubvqadgypa\ntlbmcxaelkouhsvu\nbonohfnlboxiezzr\nrktlxcbkhewyvcjl\nrsmoutcbcssodvsc\nqszdratuxcrhsvoh\neypyfahpuzqwzwhi\nyhkrleqmqlmwdnio\nvpnvxusvmngsobmq\nhkzyhopvxrsimzys\ndblriiwnrvnhxykl\nxkriqxkrprjwpncs\nrcymltrbszhyhqti\nmzbvneplsnpiztzn\nvkqtnptgbqefvfoc\nnwdtfiaozkcjtlax\ncrximadpvdaccrsm\nlrbajafxwwnxvbei\nrbexzesrytpwwmjf\nstxwjarildpnzfpg\nbtamaihdivrhhrrv\nacqbucebpaulpotl\ndkjhzghxxtxgdpvm\nrsbzwsnvlpqzyjir\nmizypbwvpgqoiams\nnvrslorjpqaasudn\nwvexcpzmconqkbvk\nrfwfumhjwzrvdzam\neaghdaqorkhdsmth\ngtuntmpqaivosewh\nnzlsmdgjrigghrmy\ndhuvxwobpzbuwjgk\nkkcuvbezftvkhebf\naeediumxyljbuyqu\nrfkpqeekjezejtjc\nwkzasuyckmgwddwy\neixpkpdhsjmynxhi\nelrlnndorggmmhmx\nayxwhkxahljoxggy\nmtzvvwmwexkberaw\nevpktriyydxvdhpx\notznecuqsfagruls\nvrdykpyebzyblnut\ncnriedolerlhbqjy\nuajaprnrrkvggqgx\nxdlxuguloojvskjq\nmfifrjamczjncuym\notmgvsykuuxrluky\noiuroieurpyejuvm")
(def input (split raw-input #"\n"))

(def vowels #{\a \e \i \o \u})

(defn contains-adjoining-dupe? [a-str]
  (= (reduce (fn [last-char curr-char]
               (if (= last-char curr-char)
                 (reduced true)
                 curr-char))
             a-str)
     true))

(defn contains-bad-substring? [a-str]
  (let [bad-substrings #{#"ab" #"cd" #"pq" #"xy"}]
    (some #(re-find % a-str) bad-substrings)))

(defn part-1-nice? [a-str]
  (and (>= (count (filter vowels a-str))
           3)
       (contains-adjoining-dupe? a-str)
       (not (contains-bad-substring? a-str))))

(s/fdef part-1-nice?
  :args (s/cat :a-str string?)
  :ret boolean?)

(def part-1-nice-strings
  (filter part-1-nice? input))

; crap this doesn't do what i thought it did

(defn find-non-overlapping-pairs
  [a-str]
  ((reduce
     (fn [state curr-char]
       (if (= (state :last-char) curr-char)
         {:instances (conj (state :instances)
                           (repeat 2 curr-char))
          :last-char nil}
         (assoc state :last-char curr-char)))

     {:instances []
      :last-char nil}

     a-str) :instances))

(s/def ::pair (s/tuple char? char?))

(s/fdef find-non-overlapping-pairs
  :args (s/cat :a-str string?)
  :ret (s/coll-of ::pair))

(defn contains-a-repetition-with-one-in-the-middle? [a-str]
  (= (reduce (fn [buffer curr-char]
               (if (< (count buffer) 3)
                 (conj buffer curr-char)

                 (let [new-buffer (conj (take 2 buffer) curr-char)]
                   (if (and (= (first new-buffer)
                               (last new-buffer))
                            (not= (first new-buffer)
                                  (second new-buffer)))
                     (reduced true)
                     new-buffer))))
             []
             a-str)
     true))

(defn part-2-nice? [a-str]
  (and
    (some #(> % 1)
          (vals (frequencies (find-non-overlapping-pairs a-str))))
    (contains-a-repetition-with-one-in-the-middle? a-str)))

(stest/instrument)

(comment
  (count (filter contains-a-repetition-with-one-in-the-middle? input))
  (count input)

  (map frequencies (filter #(> (count %) 0) (map find-non-overlapping-pairs input)))


  (find-non-overlapping-pairs "qjhvhtzxzqqjkmpb")

  (count part-1-nice-strings)

  (count (filter part-2-nice? input))

  (reductions (fn [buffer curr-char]
               (if (< (count buffer) 3)
                 (conj buffer curr-char)

                 (let [new-buffer (conj (take 2 buffer) curr-char)]
                   (if (and (= (first new-buffer)
                               (last new-buffer))
                            (not= (first new-buffer)
                                  (second new-buffer)))
                     (reduced true)
                     new-buffer))))
             []
             "mfifrjamczjncuym")

  (nice?)

  (-> []
      (conj 3)
      (conj 4))

  )