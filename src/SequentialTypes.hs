module SequentialTypes where

import CodeSyntax
import CommonTypes
import Data.Graph (Table,Vertex)
import UU.DData.Map (Map)
import qualified UU.DData.Map as Map
import Data.Maybe(fromJust)
import Data.List(partition)
import UU.Pretty

data ChildVisit = ChildVisit Name Name Int [Vertex] [Vertex] deriving (Eq,Show) -- field, rhs nt, visit nr., inh, syn
data NTAttr = NTAInh Nonterminal Name Type -- nt, attribute, type
            | NTASyn Nonterminal Name Type -- nt, attribute, type
               deriving Show

getNtaNameType (NTAInh nt name tp) = (name,tp)
getNtaNameType (NTASyn nt name tp) = (name,tp)

getAttr     (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin) = name
getIsIn     (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin) = ii
getHasCode  (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin) = hc
getLhsNt    (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin) = nt
getCon      (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin) = con
getField    (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin) = field
getRhsNt    (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin) = childnt
getType     (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin) = tp
getDefines  (CRule name ii hc nt con field childnt tp pattern rhs defines owrt origin) = defines

isLocal = (_LOC==) . getField
isLhs = (_LHS==) . getField
isRhs cr = not (isLhs cr || isLocal cr)
isSyn cr | isLocal cr  = False
         | getIsIn cr  = isRhs cr
         | otherwise   = isLhs cr
isInh = not . isSyn
hasCode cr = isLocal cr || (isLhs cr && isInh cr) || (isRhs cr && isSyn cr)

isEqualField      a b = isEqualCon a b && getField a == getField b
isDifferentField  a b = isEqualCon a b && getField a /= getField b 
isEqualCon        a b = getLhsNt a == getLhsNt b && getCon a == getCon b
isRhsOfSameCon    a b = isEqualCon a b && isRhs a && isRhs b

isSynAttr (NTAInh _ _ _) = False
isSynAttr (NTASyn _ _ _) = True
isInhAttr = not . isSynAttr

ntattr :: CRule -> Maybe NTAttr
ntattr cr  | isLocal cr =  Nothing
           | otherwise  =  let  at = if isSyn cr then NTASyn else NTAInh
                                getNt cr = if isRhs cr then fromJust (getRhsNt cr) else getLhsNt cr
                           in Just (at (getNt cr) (getAttr cr) (fromJust (getType cr)))

cRuleLhsInh :: Name -> Nonterminal -> Constructor -> Type -> CRule
cRuleLhsInh attr nt con tp = CRule attr True False nt con _LHS Nothing (Just tp) (error "cRuleLhsInh") [] Map.empty False ""
cRuleTerminal :: Name -> Nonterminal -> Constructor -> Type -> CRule
cRuleTerminal attr nt con tp = CRule attr True False nt con _LOC Nothing (Just tp) (error "cRuleTerminal") [] Map.empty False ""
cRuleRhsSyn :: Name -> Nonterminal -> Constructor -> Type -> Name -> Nonterminal -> CRule
cRuleRhsSyn attr nt con tp field childnt = CRule attr True False nt con field (Just childnt) (Just tp) (error "cRuleRhsSyn") [] Map.empty False ""

defaultRule :: Name -> Nonterminal -> Constructor -> Name -> CRule
defaultRule attr nt con field =  CRule attr (er 1) (er 2) nt con field (er 3) (er 4) (er 5) (er 6) (er 7) (er 8) (er 9)
                                 where er i = error ("Default rule has no code " ++ show i)

instance Eq CRule where
  a == b = getAttr a == getAttr b && isEqualField a b
instance Ord CRule where
  compare a b =  compare (getLhsNt a) (getLhsNt b) 
                 >/< compare (getCon a) (getCon b)
                 >/< compare (getField a) (getField b)
                 >/< compare (getAttr a) (getAttr b)
instance Eq NTAttr where
  (NTAInh nt name _) == (NTASyn nt' name' _) = False
  (NTASyn nt name _) == (NTAInh nt' name' _) = False
  (NTAInh nt name _) == (NTAInh nt' name' _) = nt == nt' && name == name'
  (NTASyn nt name _) == (NTASyn nt' name' _) = nt == nt' && name == name'
instance Ord NTAttr where
  compare (NTAInh _ _ _) (NTASyn _ _ _) = LT
  compare (NTASyn _ _ _) (NTAInh _ _ _) = GT
  compare (NTAInh nt name _) (NTAInh nt' name' _) = compare nt nt' >/< compare name name'
  compare (NTASyn nt name _) (NTASyn nt' name' _) = compare nt nt' >/< compare name name'

eqCRuleDefines :: CRule -> CRule -> Bool
eqCRuleDefines a b
  = Map.keys (getDefines a) == Map.keys (getDefines b)

(>/<) :: Ordering -> Ordering -> Ordering
EQ >/< b = b
a >/< _ = a


eqClasses :: (a -> a -> Bool) -> [a] -> [[a]]
eqClasses p [] = []
eqClasses p (a:as) = let (isA,rest) = partition (p a) as
                     in (a:isA):eqClasses p rest

lhsshow (NTAInh field attr _) = lhsname True attr
lhsshow (NTASyn field attr _) = lhsname False attr 

rhsshow :: Name -> NTAttr -> String
rhsshow field (NTAInh _ attr _) = attrname False field attr
rhsshow field (NTASyn _ attr _) = attrname True field attr 

prettyCRule :: CRule -> String
prettyCRule cr 
   =  let descr | isLocal cr = "local attribute " ++ show (getAttr cr)
                | otherwise =     (if isSyn cr then "synthesized " else "inherited ")
                               ++ "attribute "
                               ++ (if isRhs cr then show (getField cr) ++ "." else "")
                               ++ (if isLhs cr then "lhs." else "")
                               ++ (show (getAttr cr))
      in show (getLhsNt cr) ++ "." ++ show (getCon cr) ++ ", " ++ descr 
