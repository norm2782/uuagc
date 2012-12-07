

-- UUAGC 0.9.42.2 (src-ag/HsToken.ag)
module HsToken where
{-# LINE 2 "./src-ag/HsToken.ag" #-}

import CommonTypes
import UU.Scanner.Position(Pos)
{-# LINE 10 "dist/build/HsToken.hs" #-}
-- HsToken -----------------------------------------------------
{-
   alternatives:
      alternative AGLocal:
         child var            : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative AGField:
         child field          : {Identifier}
         child attr           : {Identifier}
         child pos            : {Pos}
         child rdesc          : {Maybe String}
      alternative HsToken:
         child value          : {String}
         child pos            : {Pos}
      alternative CharToken:
         child value          : {String}
         child pos            : {Pos}
      alternative StrToken:
         child value          : {String}
         child pos            : {Pos}
      alternative Err:
         child mesg           : {String}
         child pos            : {Pos}
-}
data HsToken = AGLocal (Identifier) (Pos) ((Maybe String))
             | AGField (Identifier) (Identifier) (Pos) ((Maybe String))
             | HsToken (String) (Pos)
             | CharToken (String) (Pos)
             | StrToken (String) (Pos)
             | Err (String) (Pos)
             deriving ( Show)
-- HsTokens ----------------------------------------------------
{-
   alternatives:
      alternative Cons:
         child hd             : HsToken 
         child tl             : HsTokens 
      alternative Nil:
-}
type HsTokens = [HsToken]
-- HsTokensRoot ------------------------------------------------
{-
   alternatives:
      alternative HsTokensRoot:
         child tokens         : HsTokens 
-}
data HsTokensRoot = HsTokensRoot (HsTokens)