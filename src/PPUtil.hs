module PPUtil where

--
-- Some additional pretty-print functions
-- for pretty-printing abstract syntax trees.
--

import Data.List
import qualified Data.Map as Map
import Pretty
import Options

ppListSep :: (PP s, PP c, PP o, PP a) => o -> c -> s -> [a] -> PP_Doc
ppListSep o c s pps = o >|< hlist (intersperse (pp s) (map pp pps)) >|< c

ppSpaced :: PP a => [a] -> PP_Doc
ppSpaced = ppListSep "" "" " "

ppCommas :: PP a => [a] -> PP_Doc
ppCommas = ppListSep "" "" ", "

ppVList :: PP a => [a] -> PP_Doc
ppVList []     = "[" >#< "]"
ppVList (x:xs) = vlist (("[" >#< pp x) : (map (\y -> "," >#< pp y) xs)) >#< "]"

ppMap :: (Show a, Show b) => Map.Map a b -> PP_Doc
ppMap m = ppVList [ ppF (show k) $ ppShow v | (k,v) <- Map.toList m ]

ppAssocL :: (Show a, Show b) => [(a,b)] -> PP_Doc
ppAssocL m = ppVList [ ppF (show k) $ ppShow v | (k,v) <- m ]

ppF :: String -> PP_Doc -> PP_Doc
ppF s x = s >|< ":" >#< x

ppNest :: PP a => [a] -> [PP_Doc] -> [PP_Doc] -> PP_Doc
ppNest nms attrs ps = ppNestInfo {- defaultEHCOpts -} nms attrs ps []

ppNestInfo :: PP a => {- EHCOpts -> -} [a] -> [PP_Doc] -> [PP_Doc] -> [(String,PP_Doc)] -> PP_Doc
ppNestInfo {- opts -} nms attrs ps infos
  = ppListSep "" "" "_" nms
    >#< (   (if null attrs then empty else ppSpaced attrs)
        >-< (if False {- ehcOptDebug opts -} then vlist (map (\(i,p) -> pp i >|< ":" >#< p) infos) else empty)
        )
    >-< indent 2 (vlist ps)

ppNm :: String -> PP_Doc
ppNm = text . show

ppShow :: Show x => x -> PP_Doc
ppShow x = pp $ show x

mkInfo1 :: String -> PP_Doc -> (String,PP_Doc)
mkInfo1 = (,)

ppLinePragma :: Options -> Int -> String -> PP_Doc
ppLinePragma opts ln fl
  | ocaml opts = "#" >#< show ln >#< show fl
  | clean opts = "//" >#< show ln >#< show fl
  | otherwise  = "{-# LINE" >#< show ln >#< show fl >#< "#-}"
