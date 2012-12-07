

-- UUAGC 0.9.42.2 (src-ag/Visage.ag)
module Visage where
{-# LINE 6 "./src-ag/Visage.ag" #-}

import UU.Scanner.Position(Pos(..))
import CommonTypes
import ATermAbstractSyntax
import Expression
import VisagePatterns
import VisageSyntax
import qualified Data.Map as Map
import Data.Map(Map)
import Data.List(intersperse)
import TokenDef
{-# LINE 18 "dist/build/Visage.hs" #-}

{-# LINE 2 "./src-ag/VisageSyntax.ag" #-}

import CommonTypes
import UU.Pretty
import AbstractSyntax
import VisagePatterns
import Expression
{-# LINE 27 "dist/build/Visage.hs" #-}

{-# LINE 2 "./src-ag/VisagePatterns.ag" #-}

import UU.Scanner.Position(Pos)
import CommonTypes
{-# LINE 33 "dist/build/Visage.hs" #-}

{-# LINE 2 "./src-ag/Expression.ag" #-}

import UU.Scanner.Position(Pos)
import HsToken
{-# LINE 39 "dist/build/Visage.hs" #-}
{-# LINE 19 "./src-ag/Visage.ag" #-}

convert :: String -> String
convert [] = []
convert (c:ct) | c == '\n' = '\\' : 'n' : convert ct
               | otherwise = c : convert ct
 
sQ :: String -> String
sQ []     = []
sQ (x:xs) = if (x=='"') then rest else x:rest
    where
      rest = if not (null xs) && last xs == '"' then init xs else xs

showAGPos :: Pos -> String
showAGPos (Pos l c f) | l == (-1) = ""
                      | otherwise = let file = if null f then "" else f -- No show of f
                                        lc = "(line " ++ show l ++ ", column " ++ show c ++")"
                                    in file ++ lc

showMap :: (Show a, Show b) => Map a b -> String
showMap
  = braces . concat . intersperse "," . map (uncurry assign) . Map.assocs
  where
    braces s = "{" ++ s ++ "}"
    assign a b = show a ++ ":=" ++ show b
{-# LINE 65 "dist/build/Visage.hs" #-}
-- Expression --------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative Expression:
         child pos            : {Pos}
         child tks            : {[HsToken]}
-}
-- cata
sem_Expression :: Expression ->
                  T_Expression
sem_Expression (Expression _pos _tks) =
    (sem_Expression_Expression _pos _tks)
-- semantic domain
newtype T_Expression = T_Expression (( ATerm))
data Inh_Expression = Inh_Expression {}
data Syn_Expression = Syn_Expression {aterm_Syn_Expression :: ATerm}
wrap_Expression :: T_Expression ->
                   Inh_Expression ->
                   Syn_Expression
wrap_Expression (T_Expression sem) (Inh_Expression) =
    (let ( _lhsOaterm) = sem
     in  (Syn_Expression _lhsOaterm))
sem_Expression_Expression :: Pos ->
                             ([HsToken]) ->
                             T_Expression
sem_Expression_Expression pos_ tks_ =
    (T_Expression (let _lhsOaterm :: ATerm
                       -- "./src-ag/Visage.ag"(line 103, column 17)
                       _lhsOaterm =
                           ({-# LINE 103 "./src-ag/Visage.ag" #-}
                            AAppl "Expression" [AString (sQ (showAGPos pos_)), AString (sQ (unlines . showTokens . tokensToStrings $ tks_))]
                            {-# LINE 100 "dist/build/Visage.hs" #-}
                            )
                   in  ( _lhsOaterm)))
-- VisageChild -------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VChild:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child rules          : VisageRules 
-}
-- cata
sem_VisageChild :: VisageChild ->
                   T_VisageChild
sem_VisageChild (VChild _name _tp _inh _syn _rules) =
    (sem_VisageChild_VChild _name _tp _inh _syn (sem_VisageRules _rules))
-- semantic domain
newtype T_VisageChild = T_VisageChild (( ATerm))
data Inh_VisageChild = Inh_VisageChild {}
data Syn_VisageChild = Syn_VisageChild {aterm_Syn_VisageChild :: ATerm}
wrap_VisageChild :: T_VisageChild ->
                    Inh_VisageChild ->
                    Syn_VisageChild
wrap_VisageChild (T_VisageChild sem) (Inh_VisageChild) =
    (let ( _lhsOaterm) = sem
     in  (Syn_VisageChild _lhsOaterm))
sem_VisageChild_VChild :: Identifier ->
                          Type ->
                          Attributes ->
                          Attributes ->
                          T_VisageRules ->
                          T_VisageChild
sem_VisageChild_VChild name_ tp_ inh_ syn_ (T_VisageRules rules_) =
    (T_VisageChild (let _lhsOaterm :: ATerm
                        _rulesOisLoc :: Bool
                        _rulesIaterms :: ([ATerm])
                        -- "./src-ag/Visage.ag"(line 85, column 18)
                        _lhsOaterm =
                            ({-# LINE 85 "./src-ag/Visage.ag" #-}
                             AAppl "Child" [AString (sQ (getName name_)), AString (sQ (show tp_)),
                                            AString (sQ (showMap inh_)),
                                            AString (sQ (showMap syn_)),
                                            AAppl "Rules" _rulesIaterms]
                             {-# LINE 148 "dist/build/Visage.hs" #-}
                             )
                        -- "./src-ag/Visage.ag"(line 89, column 18)
                        _rulesOisLoc =
                            ({-# LINE 89 "./src-ag/Visage.ag" #-}
                             False
                             {-# LINE 154 "dist/build/Visage.hs" #-}
                             )
                        ( _rulesIaterms) =
                            rules_ _rulesOisLoc
                    in  ( _lhsOaterm)))
-- VisageChildren ----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterms               : [ATerm]
   alternatives:
      alternative Cons:
         child hd             : VisageChild 
         child tl             : VisageChildren 
      alternative Nil:
-}
-- cata
sem_VisageChildren :: VisageChildren ->
                      T_VisageChildren
sem_VisageChildren list =
    (Prelude.foldr sem_VisageChildren_Cons sem_VisageChildren_Nil (Prelude.map sem_VisageChild list))
-- semantic domain
newtype T_VisageChildren = T_VisageChildren (( ([ATerm])))
data Inh_VisageChildren = Inh_VisageChildren {}
data Syn_VisageChildren = Syn_VisageChildren {aterms_Syn_VisageChildren :: ([ATerm])}
wrap_VisageChildren :: T_VisageChildren ->
                       Inh_VisageChildren ->
                       Syn_VisageChildren
wrap_VisageChildren (T_VisageChildren sem) (Inh_VisageChildren) =
    (let ( _lhsOaterms) = sem
     in  (Syn_VisageChildren _lhsOaterms))
sem_VisageChildren_Cons :: T_VisageChild ->
                           T_VisageChildren ->
                           T_VisageChildren
sem_VisageChildren_Cons (T_VisageChild hd_) (T_VisageChildren tl_) =
    (T_VisageChildren (let _lhsOaterms :: ([ATerm])
                           _hdIaterm :: ATerm
                           _tlIaterms :: ([ATerm])
                           -- "./src-ag/Visage.ag"(line 80, column 17)
                           _lhsOaterms =
                               ({-# LINE 80 "./src-ag/Visage.ag" #-}
                                _hdIaterm : _tlIaterms
                                {-# LINE 196 "dist/build/Visage.hs" #-}
                                )
                           ( _hdIaterm) =
                               hd_
                           ( _tlIaterms) =
                               tl_
                       in  ( _lhsOaterms)))
sem_VisageChildren_Nil :: T_VisageChildren
sem_VisageChildren_Nil =
    (T_VisageChildren (let _lhsOaterms :: ([ATerm])
                           -- "./src-ag/Visage.ag"(line 81, column 17)
                           _lhsOaterms =
                               ({-# LINE 81 "./src-ag/Visage.ag" #-}
                                []
                                {-# LINE 210 "dist/build/Visage.hs" #-}
                                )
                       in  ( _lhsOaterms)))
-- VisageGrammar -----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VGrammar:
         child nonts          : VisageNonterminals 
-}
-- cata
sem_VisageGrammar :: VisageGrammar ->
                     T_VisageGrammar
sem_VisageGrammar (VGrammar _nonts) =
    (sem_VisageGrammar_VGrammar (sem_VisageNonterminals _nonts))
-- semantic domain
newtype T_VisageGrammar = T_VisageGrammar (( ATerm))
data Inh_VisageGrammar = Inh_VisageGrammar {}
data Syn_VisageGrammar = Syn_VisageGrammar {aterm_Syn_VisageGrammar :: ATerm}
wrap_VisageGrammar :: T_VisageGrammar ->
                      Inh_VisageGrammar ->
                      Syn_VisageGrammar
wrap_VisageGrammar (T_VisageGrammar sem) (Inh_VisageGrammar) =
    (let ( _lhsOaterm) = sem
     in  (Syn_VisageGrammar _lhsOaterm))
sem_VisageGrammar_VGrammar :: T_VisageNonterminals ->
                              T_VisageGrammar
sem_VisageGrammar_VGrammar (T_VisageNonterminals nonts_) =
    (T_VisageGrammar (let _lhsOaterm :: ATerm
                          _nontsIaterms :: ([ATerm])
                          -- "./src-ag/Visage.ag"(line 54, column 18)
                          _lhsOaterm =
                              ({-# LINE 54 "./src-ag/Visage.ag" #-}
                               AAppl "Productions" _nontsIaterms
                               {-# LINE 246 "dist/build/Visage.hs" #-}
                               )
                          ( _nontsIaterms) =
                              nonts_
                      in  ( _lhsOaterm)))
-- VisageNonterminal -------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VNonterminal:
         child nt             : {NontermIdent}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child alts           : VisageProductions 
-}
-- cata
sem_VisageNonterminal :: VisageNonterminal ->
                         T_VisageNonterminal
sem_VisageNonterminal (VNonterminal _nt _inh _syn _alts) =
    (sem_VisageNonterminal_VNonterminal _nt _inh _syn (sem_VisageProductions _alts))
-- semantic domain
newtype T_VisageNonterminal = T_VisageNonterminal (( ATerm))
data Inh_VisageNonterminal = Inh_VisageNonterminal {}
data Syn_VisageNonterminal = Syn_VisageNonterminal {aterm_Syn_VisageNonterminal :: ATerm}
wrap_VisageNonterminal :: T_VisageNonterminal ->
                          Inh_VisageNonterminal ->
                          Syn_VisageNonterminal
wrap_VisageNonterminal (T_VisageNonterminal sem) (Inh_VisageNonterminal) =
    (let ( _lhsOaterm) = sem
     in  (Syn_VisageNonterminal _lhsOaterm))
sem_VisageNonterminal_VNonterminal :: NontermIdent ->
                                      Attributes ->
                                      Attributes ->
                                      T_VisageProductions ->
                                      T_VisageNonterminal
sem_VisageNonterminal_VNonterminal nt_ inh_ syn_ (T_VisageProductions alts_) =
    (T_VisageNonterminal (let _lhsOaterm :: ATerm
                              _altsIaterms :: ([ATerm])
                              -- "./src-ag/Visage.ag"(line 63, column 19)
                              _lhsOaterm =
                                  ({-# LINE 63 "./src-ag/Visage.ag" #-}
                                   AAppl "Production" [AString (sQ (getName nt_)), AString (sQ(showMap inh_)),
                                                      AString (sQ(showMap syn_)), AAppl "Alternatives" _altsIaterms]
                                   {-# LINE 291 "dist/build/Visage.hs" #-}
                                   )
                              ( _altsIaterms) =
                                  alts_
                          in  ( _lhsOaterm)))
-- VisageNonterminals ------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterms               : [ATerm]
   alternatives:
      alternative Cons:
         child hd             : VisageNonterminal 
         child tl             : VisageNonterminals 
      alternative Nil:
-}
-- cata
sem_VisageNonterminals :: VisageNonterminals ->
                          T_VisageNonterminals
sem_VisageNonterminals list =
    (Prelude.foldr sem_VisageNonterminals_Cons sem_VisageNonterminals_Nil (Prelude.map sem_VisageNonterminal list))
-- semantic domain
newtype T_VisageNonterminals = T_VisageNonterminals (( ([ATerm])))
data Inh_VisageNonterminals = Inh_VisageNonterminals {}
data Syn_VisageNonterminals = Syn_VisageNonterminals {aterms_Syn_VisageNonterminals :: ([ATerm])}
wrap_VisageNonterminals :: T_VisageNonterminals ->
                           Inh_VisageNonterminals ->
                           Syn_VisageNonterminals
wrap_VisageNonterminals (T_VisageNonterminals sem) (Inh_VisageNonterminals) =
    (let ( _lhsOaterms) = sem
     in  (Syn_VisageNonterminals _lhsOaterms))
sem_VisageNonterminals_Cons :: T_VisageNonterminal ->
                               T_VisageNonterminals ->
                               T_VisageNonterminals
sem_VisageNonterminals_Cons (T_VisageNonterminal hd_) (T_VisageNonterminals tl_) =
    (T_VisageNonterminals (let _lhsOaterms :: ([ATerm])
                               _hdIaterm :: ATerm
                               _tlIaterms :: ([ATerm])
                               -- "./src-ag/Visage.ag"(line 58, column 17)
                               _lhsOaterms =
                                   ({-# LINE 58 "./src-ag/Visage.ag" #-}
                                    _hdIaterm : _tlIaterms
                                    {-# LINE 333 "dist/build/Visage.hs" #-}
                                    )
                               ( _hdIaterm) =
                                   hd_
                               ( _tlIaterms) =
                                   tl_
                           in  ( _lhsOaterms)))
sem_VisageNonterminals_Nil :: T_VisageNonterminals
sem_VisageNonterminals_Nil =
    (T_VisageNonterminals (let _lhsOaterms :: ([ATerm])
                               -- "./src-ag/Visage.ag"(line 59, column 17)
                               _lhsOaterms =
                                   ({-# LINE 59 "./src-ag/Visage.ag" #-}
                                    []
                                    {-# LINE 347 "dist/build/Visage.hs" #-}
                                    )
                           in  ( _lhsOaterms)))
-- VisagePattern -----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VConstr:
         child name           : {ConstructorIdent}
         child pats           : VisagePatterns 
      alternative VProduct:
         child pos            : {Pos}
         child pats           : VisagePatterns 
      alternative VVar:
         child field          : {Identifier}
         child attr           : {Identifier}
      alternative VAlias:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pat            : VisagePattern 
      alternative VUnderscore:
         child pos            : {Pos}
-}
-- cata
sem_VisagePattern :: VisagePattern ->
                     T_VisagePattern
sem_VisagePattern (VConstr _name _pats) =
    (sem_VisagePattern_VConstr _name (sem_VisagePatterns _pats))
sem_VisagePattern (VProduct _pos _pats) =
    (sem_VisagePattern_VProduct _pos (sem_VisagePatterns _pats))
sem_VisagePattern (VVar _field _attr) =
    (sem_VisagePattern_VVar _field _attr)
sem_VisagePattern (VAlias _field _attr _pat) =
    (sem_VisagePattern_VAlias _field _attr (sem_VisagePattern _pat))
sem_VisagePattern (VUnderscore _pos) =
    (sem_VisagePattern_VUnderscore _pos)
-- semantic domain
newtype T_VisagePattern = T_VisagePattern (( ATerm))
data Inh_VisagePattern = Inh_VisagePattern {}
data Syn_VisagePattern = Syn_VisagePattern {aterm_Syn_VisagePattern :: ATerm}
wrap_VisagePattern :: T_VisagePattern ->
                      Inh_VisagePattern ->
                      Syn_VisagePattern
wrap_VisagePattern (T_VisagePattern sem) (Inh_VisagePattern) =
    (let ( _lhsOaterm) = sem
     in  (Syn_VisagePattern _lhsOaterm))
sem_VisagePattern_VConstr :: ConstructorIdent ->
                             T_VisagePatterns ->
                             T_VisagePattern
sem_VisagePattern_VConstr name_ (T_VisagePatterns pats_) =
    (T_VisagePattern (let _lhsOaterm :: ATerm
                          _patsIaterms :: ([ATerm])
                          -- "./src-ag/Visage.ag"(line 112, column 18)
                          _lhsOaterm =
                              ({-# LINE 112 "./src-ag/Visage.ag" #-}
                               AAppl "Pattern" [AAppl "Constr" [AString (sQ (showAGPos (getPos name_))),
                                                AString (sQ (getName name_)),
                                                AAppl "Patterns" _patsIaterms]]
                               {-# LINE 407 "dist/build/Visage.hs" #-}
                               )
                          ( _patsIaterms) =
                              pats_
                      in  ( _lhsOaterm)))
sem_VisagePattern_VProduct :: Pos ->
                              T_VisagePatterns ->
                              T_VisagePattern
sem_VisagePattern_VProduct pos_ (T_VisagePatterns pats_) =
    (T_VisagePattern (let _lhsOaterm :: ATerm
                          _patsIaterms :: ([ATerm])
                          -- "./src-ag/Visage.ag"(line 115, column 18)
                          _lhsOaterm =
                              ({-# LINE 115 "./src-ag/Visage.ag" #-}
                               AAppl "Pattern" [AAppl "Product" [AString (sQ (showAGPos pos_)),
                                                                 AAppl "Patterns" _patsIaterms]]
                               {-# LINE 423 "dist/build/Visage.hs" #-}
                               )
                          ( _patsIaterms) =
                              pats_
                      in  ( _lhsOaterm)))
sem_VisagePattern_VVar :: Identifier ->
                          Identifier ->
                          T_VisagePattern
sem_VisagePattern_VVar field_ attr_ =
    (T_VisagePattern (let _lhsOaterm :: ATerm
                          -- "./src-ag/Visage.ag"(line 117, column 18)
                          _lhsOaterm =
                              ({-# LINE 117 "./src-ag/Visage.ag" #-}
                               AAppl "Pattern" [AAppl "Var" [AString (sQ (showAGPos (getPos field_))),
                                                             AString (sQ (getName field_ ++ "." ++ getName attr_))]]
                               {-# LINE 438 "dist/build/Visage.hs" #-}
                               )
                      in  ( _lhsOaterm)))
sem_VisagePattern_VAlias :: Identifier ->
                            Identifier ->
                            T_VisagePattern ->
                            T_VisagePattern
sem_VisagePattern_VAlias field_ attr_ (T_VisagePattern pat_) =
    (T_VisagePattern (let _lhsOaterm :: ATerm
                          _patIaterm :: ATerm
                          -- "./src-ag/Visage.ag"(line 119, column 18)
                          _lhsOaterm =
                              ({-# LINE 119 "./src-ag/Visage.ag" #-}
                               AAppl "Pattern" [AAppl "Alias" [AString (sQ (showAGPos (getPos field_))),
                                                               AString (sQ (getName field_ ++ "." ++ getName attr_)), _patIaterm]]
                               {-# LINE 453 "dist/build/Visage.hs" #-}
                               )
                          ( _patIaterm) =
                              pat_
                      in  ( _lhsOaterm)))
sem_VisagePattern_VUnderscore :: Pos ->
                                 T_VisagePattern
sem_VisagePattern_VUnderscore pos_ =
    (T_VisagePattern (let _lhsOaterm :: ATerm
                          -- "./src-ag/Visage.ag"(line 121, column 18)
                          _lhsOaterm =
                              ({-# LINE 121 "./src-ag/Visage.ag" #-}
                               AAppl "Pattern" [AAppl "Underscore" [AString (sQ (showAGPos pos_))]]
                               {-# LINE 466 "dist/build/Visage.hs" #-}
                               )
                      in  ( _lhsOaterm)))
-- VisagePatterns ----------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterms               : [ATerm]
   alternatives:
      alternative Cons:
         child hd             : VisagePattern 
         child tl             : VisagePatterns 
      alternative Nil:
-}
-- cata
sem_VisagePatterns :: VisagePatterns ->
                      T_VisagePatterns
sem_VisagePatterns list =
    (Prelude.foldr sem_VisagePatterns_Cons sem_VisagePatterns_Nil (Prelude.map sem_VisagePattern list))
-- semantic domain
newtype T_VisagePatterns = T_VisagePatterns (( ([ATerm])))
data Inh_VisagePatterns = Inh_VisagePatterns {}
data Syn_VisagePatterns = Syn_VisagePatterns {aterms_Syn_VisagePatterns :: ([ATerm])}
wrap_VisagePatterns :: T_VisagePatterns ->
                       Inh_VisagePatterns ->
                       Syn_VisagePatterns
wrap_VisagePatterns (T_VisagePatterns sem) (Inh_VisagePatterns) =
    (let ( _lhsOaterms) = sem
     in  (Syn_VisagePatterns _lhsOaterms))
sem_VisagePatterns_Cons :: T_VisagePattern ->
                           T_VisagePatterns ->
                           T_VisagePatterns
sem_VisagePatterns_Cons (T_VisagePattern hd_) (T_VisagePatterns tl_) =
    (T_VisagePatterns (let _lhsOaterms :: ([ATerm])
                           _hdIaterm :: ATerm
                           _tlIaterms :: ([ATerm])
                           -- "./src-ag/Visage.ag"(line 107, column 17)
                           _lhsOaterms =
                               ({-# LINE 107 "./src-ag/Visage.ag" #-}
                                _hdIaterm : _tlIaterms
                                {-# LINE 506 "dist/build/Visage.hs" #-}
                                )
                           ( _hdIaterm) =
                               hd_
                           ( _tlIaterms) =
                               tl_
                       in  ( _lhsOaterms)))
sem_VisagePatterns_Nil :: T_VisagePatterns
sem_VisagePatterns_Nil =
    (T_VisagePatterns (let _lhsOaterms :: ([ATerm])
                           -- "./src-ag/Visage.ag"(line 108, column 17)
                           _lhsOaterms =
                               ({-# LINE 108 "./src-ag/Visage.ag" #-}
                                []
                                {-# LINE 520 "dist/build/Visage.hs" #-}
                                )
                       in  ( _lhsOaterms)))
-- VisageProduction --------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VProduction:
         child con            : {ConstructorIdent}
         child children       : VisageChildren 
         child rules          : VisageRules 
         child locrules       : VisageRules 
-}
-- cata
sem_VisageProduction :: VisageProduction ->
                        T_VisageProduction
sem_VisageProduction (VProduction _con _children _rules _locrules) =
    (sem_VisageProduction_VProduction _con (sem_VisageChildren _children) (sem_VisageRules _rules) (sem_VisageRules _locrules))
-- semantic domain
newtype T_VisageProduction = T_VisageProduction (( ATerm))
data Inh_VisageProduction = Inh_VisageProduction {}
data Syn_VisageProduction = Syn_VisageProduction {aterm_Syn_VisageProduction :: ATerm}
wrap_VisageProduction :: T_VisageProduction ->
                         Inh_VisageProduction ->
                         Syn_VisageProduction
wrap_VisageProduction (T_VisageProduction sem) (Inh_VisageProduction) =
    (let ( _lhsOaterm) = sem
     in  (Syn_VisageProduction _lhsOaterm))
sem_VisageProduction_VProduction :: ConstructorIdent ->
                                    T_VisageChildren ->
                                    T_VisageRules ->
                                    T_VisageRules ->
                                    T_VisageProduction
sem_VisageProduction_VProduction con_ (T_VisageChildren children_) (T_VisageRules rules_) (T_VisageRules locrules_) =
    (T_VisageProduction (let _lhsOaterm :: ATerm
                             _locrulesOisLoc :: Bool
                             _rulesOisLoc :: Bool
                             _childrenIaterms :: ([ATerm])
                             _rulesIaterms :: ([ATerm])
                             _locrulesIaterms :: ([ATerm])
                             -- "./src-ag/Visage.ag"(line 73, column 17)
                             _lhsOaterm =
                                 ({-# LINE 73 "./src-ag/Visage.ag" #-}
                                  AAppl "Alternative" [AString (sQ (getName con_)), AAppl "Children" _childrenIaterms,
                                                        AAppl "Rules" _rulesIaterms,
                                                        AAppl "LocRules" _locrulesIaterms]
                                  {-# LINE 568 "dist/build/Visage.hs" #-}
                                  )
                             -- "./src-ag/Visage.ag"(line 76, column 18)
                             _locrulesOisLoc =
                                 ({-# LINE 76 "./src-ag/Visage.ag" #-}
                                  True
                                  {-# LINE 574 "dist/build/Visage.hs" #-}
                                  )
                             -- "./src-ag/Visage.ag"(line 77, column 18)
                             _rulesOisLoc =
                                 ({-# LINE 77 "./src-ag/Visage.ag" #-}
                                  False
                                  {-# LINE 580 "dist/build/Visage.hs" #-}
                                  )
                             ( _childrenIaterms) =
                                 children_
                             ( _rulesIaterms) =
                                 rules_ _rulesOisLoc
                             ( _locrulesIaterms) =
                                 locrules_ _locrulesOisLoc
                         in  ( _lhsOaterm)))
-- VisageProductions -------------------------------------------
{-
   visit 0:
      synthesized attribute:
         aterms               : [ATerm]
   alternatives:
      alternative Cons:
         child hd             : VisageProduction 
         child tl             : VisageProductions 
      alternative Nil:
-}
-- cata
sem_VisageProductions :: VisageProductions ->
                         T_VisageProductions
sem_VisageProductions list =
    (Prelude.foldr sem_VisageProductions_Cons sem_VisageProductions_Nil (Prelude.map sem_VisageProduction list))
-- semantic domain
newtype T_VisageProductions = T_VisageProductions (( ([ATerm])))
data Inh_VisageProductions = Inh_VisageProductions {}
data Syn_VisageProductions = Syn_VisageProductions {aterms_Syn_VisageProductions :: ([ATerm])}
wrap_VisageProductions :: T_VisageProductions ->
                          Inh_VisageProductions ->
                          Syn_VisageProductions
wrap_VisageProductions (T_VisageProductions sem) (Inh_VisageProductions) =
    (let ( _lhsOaterms) = sem
     in  (Syn_VisageProductions _lhsOaterms))
sem_VisageProductions_Cons :: T_VisageProduction ->
                              T_VisageProductions ->
                              T_VisageProductions
sem_VisageProductions_Cons (T_VisageProduction hd_) (T_VisageProductions tl_) =
    (T_VisageProductions (let _lhsOaterms :: ([ATerm])
                              _hdIaterm :: ATerm
                              _tlIaterms :: ([ATerm])
                              -- "./src-ag/Visage.ag"(line 68, column 17)
                              _lhsOaterms =
                                  ({-# LINE 68 "./src-ag/Visage.ag" #-}
                                   _hdIaterm : _tlIaterms
                                   {-# LINE 626 "dist/build/Visage.hs" #-}
                                   )
                              ( _hdIaterm) =
                                  hd_
                              ( _tlIaterms) =
                                  tl_
                          in  ( _lhsOaterms)))
sem_VisageProductions_Nil :: T_VisageProductions
sem_VisageProductions_Nil =
    (T_VisageProductions (let _lhsOaterms :: ([ATerm])
                              -- "./src-ag/Visage.ag"(line 69, column 17)
                              _lhsOaterms =
                                  ({-# LINE 69 "./src-ag/Visage.ag" #-}
                                   []
                                   {-# LINE 640 "dist/build/Visage.hs" #-}
                                   )
                          in  ( _lhsOaterms)))
-- VisageRule --------------------------------------------------
{-
   visit 0:
      inherited attribute:
         isLoc                : Bool
      synthesized attribute:
         aterm                : ATerm
   alternatives:
      alternative VRule:
         child fieldattrs     : {[(Identifier,Identifier)]}
         child attr           : {Identifier}
         child pat            : VisagePattern 
         child rhs            : Expression 
         child owrt           : {Bool}
-}
-- cata
sem_VisageRule :: VisageRule ->
                  T_VisageRule
sem_VisageRule (VRule _fieldattrs _attr _pat _rhs _owrt) =
    (sem_VisageRule_VRule _fieldattrs _attr (sem_VisagePattern _pat) (sem_Expression _rhs) _owrt)
-- semantic domain
newtype T_VisageRule = T_VisageRule (Bool ->
                                     ( ATerm))
data Inh_VisageRule = Inh_VisageRule {isLoc_Inh_VisageRule :: Bool}
data Syn_VisageRule = Syn_VisageRule {aterm_Syn_VisageRule :: ATerm}
wrap_VisageRule :: T_VisageRule ->
                   Inh_VisageRule ->
                   Syn_VisageRule
wrap_VisageRule (T_VisageRule sem) (Inh_VisageRule _lhsIisLoc) =
    (let ( _lhsOaterm) = sem _lhsIisLoc
     in  (Syn_VisageRule _lhsOaterm))
sem_VisageRule_VRule :: ([(Identifier,Identifier)]) ->
                        Identifier ->
                        T_VisagePattern ->
                        T_Expression ->
                        Bool ->
                        T_VisageRule
sem_VisageRule_VRule fieldattrs_ attr_ (T_VisagePattern pat_) (T_Expression rhs_) owrt_ =
    (T_VisageRule (\ _lhsIisLoc ->
                       (let _lhsOaterm :: ATerm
                            _patIaterm :: ATerm
                            _rhsIaterm :: ATerm
                            -- "./src-ag/Visage.ag"(line 97, column 18)
                            _lhsOaterm =
                                ({-# LINE 97 "./src-ag/Visage.ag" #-}
                                 AAppl (if _lhsIisLoc then "LocRule" else "Rule")
                                       ([AString (sQ (getName attr_)), _patIaterm, _rhsIaterm] ++ if _lhsIisLoc then [AString (sQ (show owrt_))] else [])
                                 {-# LINE 690 "dist/build/Visage.hs" #-}
                                 )
                            ( _patIaterm) =
                                pat_
                            ( _rhsIaterm) =
                                rhs_
                        in  ( _lhsOaterm))))
-- VisageRules -------------------------------------------------
{-
   visit 0:
      inherited attribute:
         isLoc                : Bool
      synthesized attribute:
         aterms               : [ATerm]
   alternatives:
      alternative Cons:
         child hd             : VisageRule 
         child tl             : VisageRules 
      alternative Nil:
-}
-- cata
sem_VisageRules :: VisageRules ->
                   T_VisageRules
sem_VisageRules list =
    (Prelude.foldr sem_VisageRules_Cons sem_VisageRules_Nil (Prelude.map sem_VisageRule list))
-- semantic domain
newtype T_VisageRules = T_VisageRules (Bool ->
                                       ( ([ATerm])))
data Inh_VisageRules = Inh_VisageRules {isLoc_Inh_VisageRules :: Bool}
data Syn_VisageRules = Syn_VisageRules {aterms_Syn_VisageRules :: ([ATerm])}
wrap_VisageRules :: T_VisageRules ->
                    Inh_VisageRules ->
                    Syn_VisageRules
wrap_VisageRules (T_VisageRules sem) (Inh_VisageRules _lhsIisLoc) =
    (let ( _lhsOaterms) = sem _lhsIisLoc
     in  (Syn_VisageRules _lhsOaterms))
sem_VisageRules_Cons :: T_VisageRule ->
                        T_VisageRules ->
                        T_VisageRules
sem_VisageRules_Cons (T_VisageRule hd_) (T_VisageRules tl_) =
    (T_VisageRules (\ _lhsIisLoc ->
                        (let _lhsOaterms :: ([ATerm])
                             _hdOisLoc :: Bool
                             _tlOisLoc :: Bool
                             _hdIaterm :: ATerm
                             _tlIaterms :: ([ATerm])
                             -- "./src-ag/Visage.ag"(line 92, column 17)
                             _lhsOaterms =
                                 ({-# LINE 92 "./src-ag/Visage.ag" #-}
                                  _hdIaterm : _tlIaterms
                                  {-# LINE 740 "dist/build/Visage.hs" #-}
                                  )
                             -- copy rule (down)
                             _hdOisLoc =
                                 ({-# LINE 51 "./src-ag/Visage.ag" #-}
                                  _lhsIisLoc
                                  {-# LINE 746 "dist/build/Visage.hs" #-}
                                  )
                             -- copy rule (down)
                             _tlOisLoc =
                                 ({-# LINE 51 "./src-ag/Visage.ag" #-}
                                  _lhsIisLoc
                                  {-# LINE 752 "dist/build/Visage.hs" #-}
                                  )
                             ( _hdIaterm) =
                                 hd_ _hdOisLoc
                             ( _tlIaterms) =
                                 tl_ _tlOisLoc
                         in  ( _lhsOaterms))))
sem_VisageRules_Nil :: T_VisageRules
sem_VisageRules_Nil =
    (T_VisageRules (\ _lhsIisLoc ->
                        (let _lhsOaterms :: ([ATerm])
                             -- "./src-ag/Visage.ag"(line 93, column 17)
                             _lhsOaterms =
                                 ({-# LINE 93 "./src-ag/Visage.ag" #-}
                                  []
                                  {-# LINE 767 "dist/build/Visage.hs" #-}
                                  )
                         in  ( _lhsOaterms))))