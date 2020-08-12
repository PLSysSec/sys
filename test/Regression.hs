module Regression where
import           Checkers.StaticConfigs.CheckerConfigs
import           Lib
import           Utils

regressionTests :: BenchTest
regressionTests = benchTestGroup "End to end regression" [ stackUninitRegression
                                                         , heapOOBRegression
                                                         , concrRegression
                                                         , userInputRegression
                                                         ]

----
---- Stack unitialized memory checker
----

stackUninitChrome :: String
stackUninitChrome = "test/Bugs/Uninit/Chrome/"

stackUninitFirefox :: String
stackUninitFirefox = "test/Bugs/Uninit/Firefox/"

stackUninitBSD :: String
stackUninitBSD = "test/Bugs/Uninit/BSD/"

stackNumSat :: Int -> String -> [String] -> BenchTest
stackNumSat = numberSatTest uninitConfig doUninitCheck

stackUninitRegression :: BenchTest
stackUninitRegression = benchTestGroup "Stack uninit regression" [ ures
                                                                 , imgconvert
                                                                 , string
                                                                 , sk
                                                                 , cbc
                                                                 , aaa
                                                                 , client
                                                                 , serial
                                                                 , rapl
                                                                 , unified
                                                                 , mchain
                                                                 , reset
                                                                 , mbuf
                                                                 ]

-- Chrome uninit 5 bugs:
-- 2 Ures
-- one each of the rest

-- filed with external component
ures :: BenchTest
ures = stackNumSat 2 (stackUninitChrome ++ "uresbund.ll-O2_p") [".ll-O2_p"]

-- https://bugs.chromium.org/p/chromium/issues/detail?id=923799#c1
-- (want to get rid of the function, CL upstream)
imgconvert :: BenchTest
imgconvert = stackNumSat 1 (stackUninitChrome ++ "imgconvert.ll-O2_p") [".ll-O2_p"]

-- filed with external component
string :: BenchTest
string = stackNumSat 1 (stackUninitChrome ++ "StringRef.ll-Os_p") [".ll-Os_p"]

-- filed with external component
sk :: BenchTest
sk = stackNumSat 1 (stackUninitChrome ++ "SkOTUtils.ll-O2_p") [".ll-O2_p"]

-- https://bugs.chromium.org/p/chromium/issues/detail?id=1092697
cbc :: BenchTest
cbc = stackNumSat 1 (stackUninitChrome ++ "tls_cbc.ll-O2_p") [".ll-O2_p"]

-- https://bugzilla.mozilla.org/show_bug.cgi?id=1644527 (chrome report coming)
aaa :: BenchTest
aaa = stackNumSat 1 (stackUninitChrome ++ "SkScan_AAAPath.ll-O2_p") [".ll-O2_p"]

-- Firefox uninit 10 bugs:
-- 3 Prio
-- 3 png
-- 1 RAPL
-- And repeated from above: 2 Ures bugs, 1 imgconvert bug, and 1 Sk (aaa) bug

-- https://bugzilla.mozilla.org/show_bug.cgi?id=1521360
-- CVE-2019-XXXX
-- Shared bounty $1,000
client :: BenchTest
client = stackNumSat 1 (stackUninitFirefox ++ "client.ll-O2_p") [".ll-O2_p"]

-- https://bugzilla.mozilla.org/show_bug.cgi?id=1521360
-- CVE-2019-XXXX
-- Shared bounty $1,000
serial :: BenchTest
serial = stackNumSat 2 (stackUninitFirefox ++ "serial.ll-O2_p") [".ll-O2_p"]

-- https://bugzilla.mozilla.org/show_bug.cgi?id=1535880
rapl :: BenchTest
rapl = stackNumSat 1 (stackUninitFirefox ++ "rapl.ll-O2_p") [".ll-O2_p"]

-- https://bugzilla.mozilla.org/show_bug.cgi?id=1535810
-- Not fix beause its 'protected' by a null pointer dereference...
unified :: BenchTest
unified = stackNumSat 3 (stackUninitFirefox ++ "Unified_c_media_libpng0.ll-O2_p") [".ll-O2_p"]

-- BSD uninit 8 bugs:
-- 6 in mchain
-- 1 in reset
-- 1 in mbug

-- Six seperate bugs: md_get_int64be_v, md_get_int64le_v, md_get_uint16be_v
-- md_get_uint16le_v, md_get_uint32le_v, md_get_uint32be_v
-- Reported to BSD security list, still unconfirmed
mchain :: BenchTest
mchain = stackNumSat 6 (stackUninitBSD ++ "subr_mchain.ll-O2_p") [".ll-O2_p"]

-- Reported to security list, ditto
reset :: BenchTest
reset = stackNumSat 1 (stackUninitBSD ++ "ar5212_reset.ll-O2_p") [".ll-O2_p"]

-- Reported to security list, ditto
mbuf :: BenchTest
mbuf = stackNumSat 1 (stackUninitBSD ++ "xdr_mbuf.ll-O2_p") [".ll-O2_p"]

-----
----- Heap out-of-bounds checker
-----

heapOOBChrome :: String
heapOOBChrome = "test/Bugs/Malloc/Chrome/"

heapOOBFirefox :: String
heapOOBFirefox = "test/Bugs/Malloc/Firefox/"

heapOOBFalse :: String
heapOOBFalse = "test/Bugs/Malloc/False/"

heapOOBNumSat :: Int -> String -> [String] -> BenchTest
heapOOBNumSat = numberSatTest heapoobConfig doHeapOOBCheck

heapOOBRegression :: BenchTest
heapOOBRegression = benchTestGroup "Heap OOB regression" [ a_int
                                                         , invert
                                                         , openjpeg
                                                         , quote
                                                         , sqlite
                                                         , prtpool
                                                         , u_gfx
                                                         , u_hunspell
                                                         , u_media
                                                         ]

--- Chrome OOB bugs:
--- BoringSSL - 2
--- OpenJpeg - 2 (invert and OpenJpeg)
--- Nasm quote - 1 (1 false positive)
--- SQLite - 13

-- https://bugs.chromium.org/p/chromium/issues/detail?id=9XXXXX#c6
-- Two bugs, ~15 reports
a_int :: BenchTest
a_int = heapOOBNumSat 2 (heapOOBChrome ++ "a_int.ll-O1") [".ll-O1"]

-- https://bugs.chromium.org/p/chromium/issues/detail?id=9XXXXX#c3
invert :: BenchTest
invert = heapOOBNumSat 1 (heapOOBChrome ++ "invert.ll-O1") [".ll-O1"]

-- https://bugs.chromium.org/p/chromium/issues/detail?id=9XXXXX#c3
openjpeg :: BenchTest
openjpeg = heapOOBNumSat 1 (heapOOBChrome ++ "openjpeg.ll-O1") [".ll-O1"]

-- https://bugs.chromium.org/p/chromium/issues/detail?id=9XXXXX#c3
-- One bug, 1 false positive
quote :: BenchTest
quote = heapOOBNumSat 1 (heapOOBChrome ++ "quote.ll-O1") [".ll-O1"]

-- Firefox: https://bugzilla.mozilla.org/show_bug.cgi?id=1XXXXX
-- Chrome: https://bugs.chromium.org/p/chromium/issues/detail?id=XXXXX
-- CVE-2019-XXX
-- Bounty $500 (No POC, cheap bastards)
sqlite :: BenchTest
sqlite = heapOOBNumSat 13 (heapOOBChrome ++ "sqlite3_shim.ll-O1") [".ll-O1"]

--- Firefox OOB bugs (not including SQLite, which is included above):
--- 4 + SQLite

-- https://bugzilla.mozilla.org/show_bug.cgi?id=1XXXXX
prtpool :: BenchTest
prtpool = heapOOBNumSat 1 (heapOOBFirefox ++ "prtpool.ll-O1") [".ll-O1"]

-- https://bugzilla.mozilla.org/show_bug.cgi?id=1XXXXX
u_gfx :: BenchTest
u_gfx = heapOOBNumSat 1 (heapOOBFirefox ++ "Unified_cpp_gfx_gl0.ll-O1") [".ll-O1"]

-- https://bugzilla.mozilla.org/show_bug.cgi?id=1XXXXX
u_hunspell :: BenchTest
u_hunspell = heapOOBNumSat 1 (heapOOBFirefox ++ "Unified_cpp_hunspell_src0.ll-O1") [".ll-O1"]

-- https://bugzilla.mozilla.org/show_bug.cgi?id=XXXXX
u_media :: BenchTest
u_media = heapOOBNumSat 1 (heapOOBFirefox ++ "Unified_cpp_src_media-conduit0.ll-O1") [".ll-O1"]

-----
----- Concrete out-of-bounds checker
-----

concrOOBChrome :: String
concrOOBChrome = "test/Bugs/Concrete/"

concrNumSat :: Int -> String -> [String] -> BenchTest
concrNumSat = numberSatTest concroobConfig doConcrOOBCheck

concrRegression :: BenchTest
concrRegression = benchTestGroup "Concrete OOB" [ php_generator
                                                , applyEncodingHint
                                                , webm_muxer
                                                ]

-- https://bugs.chromium.org/p/chromium/issues/detail?id=9XXXXX#c1
php_generator :: BenchTest
php_generator = concrNumSat 1 (concrOOBChrome ++ "php_generator.ll-O2_p") [".ll-O2_p"]

-- https://bugs.chromium.org/p/chromium/issues/detail?id=9XXXXX#c1
-- MISSING BUG!!! ALERT ALLERT!!! MISSING BUG!!!!
applyEncodingHint :: BenchTest
applyEncodingHint = failUnimplemented "Missing bug!! Alert!"

-- https://bugs.chromium.org/p/chromium/issues/detail?id=9XXXXX#c1
-- Bounty $500
webm_muxer :: BenchTest
webm_muxer = concrNumSat 1 (concrOOBChrome ++ "webm_muxer.ll-O2_p") [".ll-O2_p"]

-----
----- User input checker
-----

userInputBSD :: String
userInputBSD = "test/Bugs/UserInput/"

userInputNumSat :: Int -> String -> [String] -> BenchTest
userInputNumSat = numberSatTest userInputConfig doUserInputCheck

userInputRegression :: BenchTest
userInputRegression = benchTestGroup "User input" [ mfi
                                                  , futexes
                                                  ]

-- Reported to BSD security list and confirmed
mfi :: BenchTest
mfi = userInputNumSat 1 (userInputBSD ++ "mfi.ll-O2_p") [".ll-O2_p"]

-- Reported to BSD security list and confirmed
futexes :: BenchTest
futexes = userInputNumSat 1 (userInputBSD ++ "linux_futex.ll-O2_p") [".ll-O2_p"]




