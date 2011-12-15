

-- UUAGC 0.9.40.1 (src-ag/Interfaces.ag)
module Interfaces where
{-# LINE 2 "src-ag/Interfaces.ag" #-}

import CommonTypes
import SequentialTypes
{-# LINE 10 "dist/build/Interfaces.hs" #-}
-- IRoot -------------------------------------------------------
{-
   alternatives:
      alternative IRoot:
         child inters         : Interfaces 
-}
data IRoot = IRoot (Interfaces)
-- Interface ---------------------------------------------------
{-
   alternatives:
      alternative Interface:
         child nt             : {NontermIdent}
         child cons           : {[ConstructorIdent]}
         child seg            : Segments 
-}
data Interface = Interface (NontermIdent) (([ConstructorIdent])) (Segments)
-- Interfaces --------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Interface 
         child tl             : Interfaces 
      alternative Nil:
-}
type Interfaces = [Interface]
-- Segment -----------------------------------------------------
{-
   alternatives:
      alternative Segment:
         child inh            : {[Vertex]}
         child syn            : {[Vertex]}
-}
data Segment = Segment (([Vertex])) (([Vertex]))
-- Segments ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : Segment 
         child tl             : Segments 
      alternative Nil:
-}
type Segments = [Segment]