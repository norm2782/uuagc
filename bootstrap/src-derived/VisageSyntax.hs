

-- UUAGC 0.9.39.0.0 (src-ag/VisageSyntax.ag)
module VisageSyntax where
{-# LINE 2 "src-ag/VisageSyntax.ag" #-}

import CommonTypes
import UU.Pretty
import AbstractSyntax
import VisagePatterns
import Expression
{-# LINE 13 "dist/build/uuagc/uuagc-tmp/VisageSyntax.hs" #-}
-- VisageChild -------------------------------------------------
{-
   alternatives:
      alternative VChild:
         child name           : {Identifier}
         child tp             : {Type}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child rules          : VisageRules 
-}
data VisageChild  = VChild (Identifier) (Type) (Attributes) (Attributes) (VisageRules ) 
-- VisageChildren ----------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : VisageChild 
         child tl             : VisageChildren 
      alternative Nil:
-}
type VisageChildren  = [VisageChild ]
-- VisageGrammar -----------------------------------------------
{-
   alternatives:
      alternative VGrammar:
         child nonts          : VisageNonterminals 
-}
data VisageGrammar  = VGrammar (VisageNonterminals ) 
-- VisageNonterminal -------------------------------------------
{-
   alternatives:
      alternative VNonterminal:
         child nt             : {NontermIdent}
         child inh            : {Attributes}
         child syn            : {Attributes}
         child alts           : VisageProductions 
-}
data VisageNonterminal  = VNonterminal (NontermIdent) (Attributes) (Attributes) (VisageProductions ) 
-- VisageNonterminals ------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : VisageNonterminal 
         child tl             : VisageNonterminals 
      alternative Nil:
-}
type VisageNonterminals  = [VisageNonterminal ]
-- VisageProduction --------------------------------------------
{-
   alternatives:
      alternative VProduction:
         child con            : {ConstructorIdent}
         child children       : VisageChildren 
         child rules          : VisageRules 
         child locrules       : VisageRules 
-}
data VisageProduction  = VProduction (ConstructorIdent) (VisageChildren ) (VisageRules ) (VisageRules ) 
-- VisageProductions -------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : VisageProduction 
         child tl             : VisageProductions 
      alternative Nil:
-}
type VisageProductions  = [VisageProduction ]
-- VisageRule --------------------------------------------------
{-
   alternatives:
      alternative VRule:
         child fieldattrs     : {[(Identifier,Identifier)]}
         child attr           : {Identifier}
         child pat            : {VisagePattern}
         child rhs            : {Expression}
         child owrt           : {Bool}
-}
data VisageRule  = VRule (([(Identifier,Identifier)])) (Identifier) (VisagePattern) (Expression) (Bool) 
-- VisageRules -------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : VisageRule 
         child tl             : VisageRules 
      alternative Nil:
-}
type VisageRules  = [VisageRule ]