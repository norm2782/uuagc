

-- UUAGC 0.9.42.2 (src-ag/PrintErrorMessages.ag)
module PrintErrorMessages where
{-# LINE 4 "./src-ag/PrintErrorMessages.ag" #-}

import UU.Scanner.Position(Pos(..), noPos)
import ErrorMessages
import Data.List(mapAccumL)
import GrammarInfo
import qualified Control.Monad.Error.Class as Err
{-# LINE 13 "dist/build/PrintErrorMessages.hs" #-}

{-# LINE 2 "./src-ag/ErrorMessages.ag" #-}

import UU.Scanner.Position(Pos)
import Pretty
import CodeSyntax
import CommonTypes
{-# LINE 21 "dist/build/PrintErrorMessages.hs" #-}
{-# LINE 13 "./src-ag/PrintErrorMessages.ag" #-}

instance Err.Error Error where
  noMsg  = Err.strMsg "error"
  strMsg = CustomError False noPos . pp
{-# LINE 27 "dist/build/PrintErrorMessages.hs" #-}

{-# LINE 20 "./src-ag/PrintErrorMessages.ag" #-}

isError :: Options -> Error -> Bool
isError _ (ParserError     _ _ _    ) = True
isError _ (DupAlt          _ _ _    ) = False
isError _ (DupSynonym      _ _      ) = False
isError _ (DupSet          _ _      ) = False
isError _ (DupInhAttr      _ _ _    ) = True
isError _ (DupSynAttr      _ _ _    ) = True
isError _ (DupChild        _ _ _ _  ) = False
isError _ (DupRule         _ _ _ _ _) = True
isError _ (DupSig          _ _ _    ) = False
isError _ (UndefNont       _        ) = True
isError _ (UndefAlt        _ _      ) = True
isError _ (UndefChild      _ _ _    ) = True
isError _ (MissingRule     _ _ _ _  ) = False
isError _ (SuperfluousRule _ _ _ _  ) = False
isError _ (UndefLocal      _ _ _    ) = True
isError _ (ChildAsLocal    _ _ _    ) = False
isError _ (UndefAttr       _ _ _ _ _) = True
isError _ (CyclicSet       _        ) = True
isError _ (CustomError     w _ _    ) = not w
isError opts (LocalCirc       _ _ _ _ _) = cycleIsDangerous opts
isError opts (InstCirc        _ _ _ _ _) = cycleIsDangerous opts
isError opts (DirectCirc      _ _ _    ) = cycleIsDangerous opts
isError opts (InducedCirc     _ _ _    ) = cycleIsDangerous opts
isError _ (MissingTypeSig  _ _ _    ) = False
isError _ (MissingInstSig  _ _ _    ) = True
isError _ (DupUnique       _ _ _    ) = False
isError _ (MissingUnique   _ _      ) = True
isError _ (MissingSyn      _ _      ) = True
isError _ (MissingNamedRule _ _ _)    = True
isError _ (DupRuleName _ _ _)         = True
isError _ (HsParseError _ _)          = True
isError _ (Cyclic _ _ _)              = True
isError _ (IncompatibleVisitKind _ _ _ _) = True
isError _ (IncompatibleRuleKind _ _)      = True
isError _ (IncompatibleAttachKind _ _)    = True

cycleIsDangerous :: Options -> Bool
cycleIsDangerous opts
  = any ($ opts) [ wignore, bangpats, cases, strictCases, stricterCases, strictSems, withCycle ]
{-# LINE 71 "dist/build/PrintErrorMessages.hs" #-}

{-# LINE 548 "./src-ag/PrintErrorMessages.ag" #-}

toWidth :: Int -> String -> String
toWidth n xs | k<n       = xs ++ replicate (n-k) ' '
             | otherwise = xs
               where k = length xs

showEdge :: ((Identifier,Identifier),[String],[String]) -> PP_Doc
showEdge ((inh,syn),_,_)
  = text ("inherited attribute " ++ toWidth 20 (getName inh) ++ " with synthesized attribute " ++  getName syn)

showEdgeLong :: ((Identifier,Identifier),[String],[String]) -> PP_Doc
showEdgeLong ((inh,syn),path1,path2)
  = text ("inherited attribute " ++ getName inh ++ " is needed for " ++  "synthesized attribute " ++ getName syn)
    >-< indent 4 (vlist (map text path2))
    >-< text "and back: "
    >-< indent 4 (vlist (map text path1))

attrText :: Identifier -> Identifier -> String
attrText inh syn
 = 	if   inh == syn
    then "threaded attribute " ++ getName inh
    else "inherited attribute " ++ getName inh ++ " and synthesized attribute " ++getName syn

showLineNr :: Int -> String
showLineNr i | i==(-1) = "CR"
             | otherwise = show i

showAttrDef :: Identifier -> Identifier -> String
showAttrDef f a | f == _LHS  = "synthesized attribute " ++ getName a
                | f == _LOC  = "local attribute " ++ getName a
                | f == _INST = "inst attribute " ++ getName a
                | otherwise  = "inherited attribute " ++ getName a ++ " of field " ++ getName f

showAttrUse :: Identifier -> Identifier -> String
showAttrUse f a | f == _LHS  = "inherited attribute " ++ getName a
                | f == _LOC  = "local attribute " ++ getName a
                | f == _INST = "inst attribute " ++ getName a
                | otherwise  = "synthesized attribute " ++ getName a ++ " of field " ++ getName f

ppAttr :: Identifier -> Identifier -> PP_Doc
ppAttr f a = text (getName f++"."++getName a)
ppAttrUse :: Identifier -> Identifier -> PP_Doc
ppAttrUse f a = "@" >|< ppAttr f a
{-# LINE 117 "dist/build/PrintErrorMessages.hs" #-}

{-# LINE 594 "./src-ag/PrintErrorMessages.ag" #-}

infixr 5 +#+
(+#+) :: String -> String -> String
(+#+) s t = s ++ " " ++ t

infixr 5 +.+
(+.+) :: Identifier -> Identifier -> String
(+.+) s t = getName s ++ "." ++ getName t

wfill :: [String] -> PP_Doc
wfill = fill . addSpaces. concat . map words
  where addSpaces (x:xs) = x:map addSpace xs
        addSpaces []     = []
        addSpace  [x]    | x `elem` ".,;:!?" = [x]
        addSpace  xs     = ' ':xs

ppError :: Bool           -- class of the error, True:error False:warning
        -> Pos      -- source position
        -> PP_Doc         -- error message
        -> PP_Doc         -- pattern
        -> PP_Doc         -- help, more info
        -> PP_Doc         -- action taken by AG
        -> Bool           -- verbose? show help and action?
        -> PP_Doc
ppError isErr pos mesg pat hlp act verb
  = let position = case pos of
                     Pos l c f | l >= 0    -> f >|< ":" >|< show l >|< ":" >|< show c
                               | otherwise -> pp "uuagc"
        tp      = if isErr then "error" else "warning"
        header  = position >|< ":" >#< tp >|< ":" >#< mesg
        pattern = "pattern  :" >#< pat
        help    = "help     :" >#< hlp
        action  = "action   :" >#< act
    in if verb
         then vlist [text "",header,pattern,help,action]
         else header

{-
-- old error reporting code
  = let
      cl = if isError then "ERROR" else "Warning"
      position   = case pos of
                         (Pos l c f) | l >= 0    -> f >|< ": line " >|< show l >|< ", column " >|< show c
                                     | otherwise -> empty
      header     = "*** UU.AG" >#< cl >#< position >#< "***"
      message    = "problem  :" >#< mesg
      pattern    = "pattern  :" >#< pat
      help       = "help     :" >#< hlp
      action     = "action   :" >#< act
    in
      if verbose
         then vlist [text "",header,message,pattern,help,action]
         else vlist [text "",header,message]
-}

showPos :: Identifier -> String
showPos = show . getPos

ppInterface :: Show a => a -> PP_Doc
ppInterface inter = wfill ["interface:", show inter]

{-# LINE 181 "dist/build/PrintErrorMessages.hs" #-}
-- Error -------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         options              : Options
         verbose              : Bool
      synthesized attributes:
         me                   : Error 
         pp                   : PP_Doc
   alternatives:
      alternative ParserError:
         child pos            : {Pos}
         child problem        : {String}
         child action         : {String}
         visit 0:
            local me          : _
      alternative HsParseError:
         child pos            : {Pos}
         child msg            : {String}
         visit 0:
            local me          : _
      alternative DupAlt:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child occ1           : {ConstructorIdent}
         visit 0:
            local me          : _
      alternative DupSynonym:
         child nt             : {NontermIdent}
         child occ1           : {NontermIdent}
         visit 0:
            local me          : _
      alternative DupSet:
         child name           : {NontermIdent}
         child occ1           : {NontermIdent}
         visit 0:
            local me          : _
      alternative DupInhAttr:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         child occ1           : {Identifier}
         visit 0:
            local me          : _
      alternative DupSynAttr:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         child occ1           : {Identifier}
         visit 0:
            local me          : _
      alternative DupChild:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child name           : {Identifier}
         child occ1           : {Identifier}
         visit 0:
            local me          : _
      alternative DupRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         child occ1           : {Identifier}
         visit 0:
            local me          : _
      alternative DupRuleName:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child nm             : {Identifier}
         visit 0:
            local me          : _
      alternative DupSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative UndefNont:
         child nt             : {NontermIdent}
         visit 0:
            local me          : _
      alternative UndefAlt:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         visit 0:
            local me          : _
      alternative UndefChild:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child name           : {Identifier}
         visit 0:
            local me          : _
      alternative MissingRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative MissingNamedRule:
         child nt             : {NontermIdent}
         child con            : {Identifier}
         child name           : {Identifier}
         visit 0:
            local me          : _
      alternative SuperfluousRule:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative UndefLocal:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child var            : {Identifier}
         visit 0:
            local me          : _
      alternative ChildAsLocal:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child var            : {Identifier}
         visit 0:
            local me          : _
      alternative UndefAttr:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child field          : {Identifier}
         child attr           : {Identifier}
         child isOut          : {Bool}
         visit 0:
            local me          : _
      alternative Cyclic:
         child nt             : {NontermIdent}
         child mbCon          : {Maybe ConstructorIdent}
         child verts          : {[String]}
         visit 0:
            local me          : _
      alternative CyclicSet:
         child name           : {Identifier}
         visit 0:
            local me          : _
      alternative CustomError:
         child isWarning      : {Bool}
         child pos            : {Pos}
         child mesg           : {PP_Doc}
         visit 0:
            local me          : _
      alternative LocalCirc:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         child o_visit        : {Bool}
         child path           : {[String]}
         visit 0:
            local me          : _
      alternative InstCirc:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         child o_visit        : {Bool}
         child path           : {[String]}
         visit 0:
            local me          : _
      alternative DirectCirc:
         child nt             : {NontermIdent}
         child o_visit        : {Bool}
         child cyclic         : {[((Identifier,Identifier),[String],[String])]}
         visit 0:
            local me          : _
      alternative InducedCirc:
         child nt             : {NontermIdent}
         child cinter         : {CInterface}
         child cyclic         : {[((Identifier,Identifier),[String],[String])]}
         visit 0:
            local me          : _
      alternative MissingTypeSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative MissingInstSig:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative DupUnique:
         child nt             : {NontermIdent}
         child con            : {ConstructorIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative MissingUnique:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative MissingSyn:
         child nt             : {NontermIdent}
         child attr           : {Identifier}
         visit 0:
            local me          : _
      alternative IncompatibleVisitKind:
         child child          : {Identifier}
         child vis            : {VisitIdentifier}
         child from           : {VisitKind}
         child to             : {VisitKind}
         visit 0:
            local me          : _
      alternative IncompatibleRuleKind:
         child rule           : {Identifier}
         child kind           : {VisitKind}
         visit 0:
            local me          : _
      alternative IncompatibleAttachKind:
         child child          : {Identifier}
         child kind           : {VisitKind}
         visit 0:
            local me          : _
-}
-- cata
sem_Error :: Error ->
             T_Error
sem_Error (ParserError _pos _problem _action) =
    (sem_Error_ParserError _pos _problem _action)
sem_Error (HsParseError _pos _msg) =
    (sem_Error_HsParseError _pos _msg)
sem_Error (DupAlt _nt _con _occ1) =
    (sem_Error_DupAlt _nt _con _occ1)
sem_Error (DupSynonym _nt _occ1) =
    (sem_Error_DupSynonym _nt _occ1)
sem_Error (DupSet _name _occ1) =
    (sem_Error_DupSet _name _occ1)
sem_Error (DupInhAttr _nt _attr _occ1) =
    (sem_Error_DupInhAttr _nt _attr _occ1)
sem_Error (DupSynAttr _nt _attr _occ1) =
    (sem_Error_DupSynAttr _nt _attr _occ1)
sem_Error (DupChild _nt _con _name _occ1) =
    (sem_Error_DupChild _nt _con _name _occ1)
sem_Error (DupRule _nt _con _field _attr _occ1) =
    (sem_Error_DupRule _nt _con _field _attr _occ1)
sem_Error (DupRuleName _nt _con _nm) =
    (sem_Error_DupRuleName _nt _con _nm)
sem_Error (DupSig _nt _con _attr) =
    (sem_Error_DupSig _nt _con _attr)
sem_Error (UndefNont _nt) =
    (sem_Error_UndefNont _nt)
sem_Error (UndefAlt _nt _con) =
    (sem_Error_UndefAlt _nt _con)
sem_Error (UndefChild _nt _con _name) =
    (sem_Error_UndefChild _nt _con _name)
sem_Error (MissingRule _nt _con _field _attr) =
    (sem_Error_MissingRule _nt _con _field _attr)
sem_Error (MissingNamedRule _nt _con _name) =
    (sem_Error_MissingNamedRule _nt _con _name)
sem_Error (SuperfluousRule _nt _con _field _attr) =
    (sem_Error_SuperfluousRule _nt _con _field _attr)
sem_Error (UndefLocal _nt _con _var) =
    (sem_Error_UndefLocal _nt _con _var)
sem_Error (ChildAsLocal _nt _con _var) =
    (sem_Error_ChildAsLocal _nt _con _var)
sem_Error (UndefAttr _nt _con _field _attr _isOut) =
    (sem_Error_UndefAttr _nt _con _field _attr _isOut)
sem_Error (Cyclic _nt _mbCon _verts) =
    (sem_Error_Cyclic _nt _mbCon _verts)
sem_Error (CyclicSet _name) =
    (sem_Error_CyclicSet _name)
sem_Error (CustomError _isWarning _pos _mesg) =
    (sem_Error_CustomError _isWarning _pos _mesg)
sem_Error (LocalCirc _nt _con _attr _o_visit _path) =
    (sem_Error_LocalCirc _nt _con _attr _o_visit _path)
sem_Error (InstCirc _nt _con _attr _o_visit _path) =
    (sem_Error_InstCirc _nt _con _attr _o_visit _path)
sem_Error (DirectCirc _nt _o_visit _cyclic) =
    (sem_Error_DirectCirc _nt _o_visit _cyclic)
sem_Error (InducedCirc _nt _cinter _cyclic) =
    (sem_Error_InducedCirc _nt _cinter _cyclic)
sem_Error (MissingTypeSig _nt _con _attr) =
    (sem_Error_MissingTypeSig _nt _con _attr)
sem_Error (MissingInstSig _nt _con _attr) =
    (sem_Error_MissingInstSig _nt _con _attr)
sem_Error (DupUnique _nt _con _attr) =
    (sem_Error_DupUnique _nt _con _attr)
sem_Error (MissingUnique _nt _attr) =
    (sem_Error_MissingUnique _nt _attr)
sem_Error (MissingSyn _nt _attr) =
    (sem_Error_MissingSyn _nt _attr)
sem_Error (IncompatibleVisitKind _child _vis _from _to) =
    (sem_Error_IncompatibleVisitKind _child _vis _from _to)
sem_Error (IncompatibleRuleKind _rule _kind) =
    (sem_Error_IncompatibleRuleKind _rule _kind)
sem_Error (IncompatibleAttachKind _child _kind) =
    (sem_Error_IncompatibleAttachKind _child _kind)
-- semantic domain
newtype T_Error = T_Error (Options ->
                           Bool ->
                           ( Error,PP_Doc))
data Inh_Error = Inh_Error {options_Inh_Error :: Options,verbose_Inh_Error :: Bool}
data Syn_Error = Syn_Error {me_Syn_Error :: Error,pp_Syn_Error :: PP_Doc}
wrap_Error :: T_Error ->
              Inh_Error ->
              Syn_Error
wrap_Error (T_Error sem) (Inh_Error _lhsIoptions _lhsIverbose) =
    (let ( _lhsOme,_lhsOpp) = sem _lhsIoptions _lhsIverbose
     in  (Syn_Error _lhsOme _lhsOpp))
sem_Error_ParserError :: Pos ->
                         String ->
                         String ->
                         T_Error
sem_Error_ParserError pos_ problem_ action_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 87, column 21)
                       _lhsOpp =
                           ({-# LINE 87 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg = text ("parser expecting " ++ problem_)
                                pat  = text ""
                                help = text ""
                                act  = text action_
                             in ppError (isError _lhsIoptions _me) pos_ mesg pat help act _lhsIverbose
                            {-# LINE 505 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            ParserError pos_ problem_ action_
                            {-# LINE 511 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 517 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_HsParseError :: Pos ->
                          String ->
                          T_Error
sem_Error_HsParseError pos_ msg_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 93, column 21)
                       _lhsOpp =
                           ({-# LINE 93 "./src-ag/PrintErrorMessages.ag" #-}
                            ppError True pos_ (text msg_) (text "") (text "") (text "Correct the syntax of the Haskell code.") _lhsIverbose
                            {-# LINE 532 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            HsParseError pos_ msg_
                            {-# LINE 538 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 544 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DupAlt :: NontermIdent ->
                    ConstructorIdent ->
                    ConstructorIdent ->
                    T_Error
sem_Error_DupAlt nt_ con_ occ1_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 95, column 21)
                       _lhsOpp =
                           ({-# LINE 95 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Repeated definition for alternative", getName con_
                                              ,"of nonterminal", getName nt_, "."
                                              ] >-<
                                        wfill ["First definition:", (showPos occ1_),"."] >-<
                                        wfill ["Other definition:", (showPos con_),"."]
                                pat =     "DATA" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< "...")
                                      >-< indent 2 ("|" >#< getName con_ >#< "...")
                                help =  wfill ["The nonterminal",getName nt_,"has more than one alternative that"
                                              ,"is labelled with the constructor name",getName con_,"."
                                              ,"You should either rename or remove enough of them to make all"
                                              ,"constructors of",getName nt_,"uniquely named."
                                              ]
                                act  = wfill [ "The first alternative of name",getName con_
                                             ,"you have given for nonterminal",getName nt_
                                             ,"is considered valid. All other alternatives have been discarded."
                                             ]
                            in ppError (isError _lhsIoptions _me) (getPos con_) mesg pat help act _lhsIverbose
                            {-# LINE 577 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DupAlt nt_ con_ occ1_
                            {-# LINE 583 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 589 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DupSynonym :: NontermIdent ->
                        NontermIdent ->
                        T_Error
sem_Error_DupSynonym nt_ occ1_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 117, column 21)
                       _lhsOpp =
                           ({-# LINE 117 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Definition of type synonym", getName nt_, "clashes with another"
                                              ,"type synonym."
                                              ] >-<
                                        wfill ["First definition:", (showPos occ1_),"."] >-<
                                        wfill ["Type synonym :"   , (showPos nt_),"."]
                                pat =     "DATA" >#< getName nt_
                                      >-< indent 2 ("|" >#< "...")
                                      >-< "TYPE" >#< getName nt_ >#< "=" >#<  "..."
                                help =  wfill ["A type synonym with name", getName  nt_
                                              ,"has been given while there already is TYPE"
                                              ,"definition with the same name."
                                              ,"You should either rename or remove the type synonym."
                                              ]
                                act  = wfill [ "The clashing type synonym will be ignored."
                                             ]
                            in ppError (isError _lhsIoptions _me)  (getPos nt_) mesg pat help act _lhsIverbose
                            {-# LINE 619 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DupSynonym nt_ occ1_
                            {-# LINE 625 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 631 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DupSet :: NontermIdent ->
                    NontermIdent ->
                    T_Error
sem_Error_DupSet name_ occ1_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 134, column 21)
                       _lhsOpp =
                           ({-# LINE 134 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Definition of nonterminal set", getName name_, "clashes with another"
                                              ,"set, a type synonym or a data definition."
                                              ] >-<
                                        wfill ["First definition:", (showPos occ1_),"."] >-<
                                        wfill ["Set definition:"   , (showPos name_),"."]
                                pat =     "SET" >#< getName name_ >#< "=" >#<  "..."
                                      >-< "SET" >#< getName name_ >#< "=" >#<  "..."
                                help =  wfill ["A nonterminal set with name", getName  name_
                                              ,"has been given while there already is a SET, DATA, or TYPE"
                                              ,"definition with the same name."
                                              ,"You should either rename or remove the nonterminal set."
                                              ]
                                act  = wfill [ "The clashing nonterminal set will be ignored."
                                             ]
                            in ppError (isError _lhsIoptions _me)  (getPos name_) mesg pat help act _lhsIverbose
                            {-# LINE 660 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DupSet name_ occ1_
                            {-# LINE 666 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 672 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DupInhAttr :: NontermIdent ->
                        Identifier ->
                        Identifier ->
                        T_Error
sem_Error_DupInhAttr nt_ attr_ occ1_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 150, column 21)
                       _lhsOpp =
                           ({-# LINE 150 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Repeated declaration of inherited attribute", getName attr_
                                              , "of nonterminal", getName nt_, "."
                                              ] >-<
                                        wfill ["First definition:", (showPos occ1_),"."] >-<
                                        wfill ["Other definition:", (showPos attr_),"."]
                                pat  = "ATTR" >#< getName nt_ >#< "[" >#< getName attr_ >|< ":...,"
                                                              >#< getName attr_ >|< ":... | | ]"
                                help =  wfill ["The identifier" , getName attr_ ,"has been declared"
                                              ,"as an inherited (or chained) attribute for nonterminal"
                                              ,getName nt_ , "more than once, with possibly different types."
                                              ,"Delete all but one or rename them to make them unique."
                                              ]
                                act  = wfill ["One declaration with its corresponding type is considered valid."
                                             ,"All others have been discarded. The generated program will probably not run."
                                             ]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 703 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DupInhAttr nt_ attr_ occ1_
                            {-# LINE 709 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 715 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DupSynAttr :: NontermIdent ->
                        Identifier ->
                        Identifier ->
                        T_Error
sem_Error_DupSynAttr nt_ attr_ occ1_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 169, column 21)
                       _lhsOpp =
                           ({-# LINE 169 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Repeated declaration of synthesized attribute", getName attr_
                                              , "of nonterminal", getName nt_, "."
                                              ] >-<
                                        wfill ["First definition:", (showPos occ1_),"."] >-<
                                        wfill ["Other definition:", (showPos attr_),"."]
                                pat  = "ATTR" >#< getName nt_ >#< "[ | |" >#< getName attr_ >|< ":...,"
                                                                  >#< getName attr_ >|< ":... ]"
                                help =  wfill ["The identifier" , getName attr_ ,"has been declared"
                                              ,"as a synthesized (or chained) attribute for nonterminal"
                                              ,getName nt_ , "more than once, with possibly different types."
                                              ,"Delete all but one or rename them to make them unique."
                                              ]
                                act  = wfill ["One declaration with its corresponding type is considered valid."
                                             ,"All others have been discarded. The generated program will probably not run."
                                             ]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 746 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DupSynAttr nt_ attr_ occ1_
                            {-# LINE 752 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 758 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DupChild :: NontermIdent ->
                      ConstructorIdent ->
                      Identifier ->
                      Identifier ->
                      T_Error
sem_Error_DupChild nt_ con_ name_ occ1_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 188, column 21)
                       _lhsOpp =
                           ({-# LINE 188 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Repeated declaration for field", getName name_, "of alternative"
                                              ,getName con_, "of nonterminal", getName nt_, "."
                                              ] >-<
                                        wfill ["First definition:", (showPos occ1_),"."] >-<
                                        wfill ["Other definition:", (showPos name_),"."]
                                pat   =   "DATA" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< (getName name_ >|< ":..." >-< getName name_ >|< ":..."))
                                help =  wfill ["The alternative" ,getName con_ , "of nonterminal" ,getName nt_
                                              ,"has more than one field that is named"
                                              , getName name_ ++ ". Possibly they have different types."
                                              ,"You should either rename or remove enough of them to make all fields of"
                                              ,getName con_ , "for nonterminal " , getName nt_ , "uniquely named."
                                              ]
                                act  = wfill ["The last declaration with its corresponding type is considered valid."
                                             ,"All others have been discarded."
                                             ]
                            in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                            {-# LINE 791 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DupChild nt_ con_ name_ occ1_
                            {-# LINE 797 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 803 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DupRule :: NontermIdent ->
                     ConstructorIdent ->
                     Identifier ->
                     Identifier ->
                     Identifier ->
                     T_Error
sem_Error_DupRule nt_ con_ field_ attr_ occ1_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 208, column 21)
                       _lhsOpp =
                           ({-# LINE 208 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more rules for"
                                              ,showAttrDef field_ attr_,"."
                                              ]  >-<
                                        wfill ["First rule:", (showPos occ1_),"."] >-<
                                        wfill ["Other rule:", (showPos attr_),"."]
                                pat   =   "SEM" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                                      >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                                help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                      ,", there is more than one rule for the" , showAttrDef field_ attr_
                                                      ,". You should either rename or remove enough of them to make all rules for alternative"
                                                      ,getName con_ , "of nonterminal " ,getName  nt_ , "uniquely named."
                                                      ]
                                act  = wfill ["The last rule given is considered valid. All others have been discarded."]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 835 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DupRule nt_ con_ field_ attr_ occ1_
                            {-# LINE 841 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 847 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DupRuleName :: NontermIdent ->
                         ConstructorIdent ->
                         Identifier ->
                         T_Error
sem_Error_DupRuleName nt_ con_ nm_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 226, column 21)
                       _lhsOpp =
                           ({-# LINE 226 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more rule names for"
                                              ,show nm_,"."
                                              ]
                                pat   =   "SEM" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< show nm_ >#< ": ... = ...")
                                      >-< indent 2 ("|" >#< getName con_ >#< show nm_ >#< ": ... = ...")
                                help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                      ,", there is more than one rule name " , show nm_
                                                      ,". You should either rename or remove enough of them."
                                                      ]
                                act  = wfill ["Compilation cannot continue."]
                            in ppError (isError _lhsIoptions _me) (getPos nm_) mesg pat help act _lhsIverbose
                            {-# LINE 874 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DupRuleName nt_ con_ nm_
                            {-# LINE 880 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 886 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DupSig :: NontermIdent ->
                    ConstructorIdent ->
                    Identifier ->
                    T_Error
sem_Error_DupSig nt_ con_ attr_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 241, column 21)
                       _lhsOpp =
                           ({-# LINE 241 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more typesignatures for"
                                              ,showAttrDef _LOC attr_,"."
                                              ]  >-<
                                        wfill ["First signature:", (showPos attr_),"."]
                                pat   =   "SEM" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< "= ...")
                                      >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< "= ...")
                                help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                      ,", there is more than one rule for the" , showAttrDef _LOC attr_
                                                      ,". You should remove enough of them to make all typesignatures for alternative"
                                                      ,getName con_ , "of nonterminal " ,getName  nt_ , "unique."
                                                      ]
                                act  = wfill ["The last typesignature given is considered valid. All others have been discarded."]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 915 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DupSig nt_ con_ attr_
                            {-# LINE 921 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 927 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_UndefNont :: NontermIdent ->
                       T_Error
sem_Error_UndefNont nt_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 258, column 21)
                       _lhsOpp =
                           ({-# LINE 258 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Nonterminal", getName nt_, "is not defined."
                                              ]
                                pat   = "DATA" >#< getName nt_ >#< "..."
                                help =  wfill ["There are attributes and/or rules for nonterminal" , getName nt_  ,", but there is no definition"
                                                      , "for" ,getName  nt_, ". Maybe you misspelled it? Otherwise insert a definition."
                                                      ]
                                act  = wfill ["Everything regarding the unknown nonterminal has been ignored."]
                            in ppError (isError _lhsIoptions _me) (getPos nt_) mesg pat help act _lhsIverbose
                            {-# LINE 948 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            UndefNont nt_
                            {-# LINE 954 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 960 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_UndefAlt :: NontermIdent ->
                      ConstructorIdent ->
                      T_Error
sem_Error_UndefAlt nt_ con_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 268, column 21)
                       _lhsOpp =
                           ({-# LINE 268 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Constructor", getName con_, "of nonterminal" ,getName nt_, "is  not defined."
                                              ]
                                pat   =   "DATA" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< "...")
                                help =  wfill ["There are rules for alternative", getName con_ , "of nonterminal" ,getName nt_
                                                      ,", but there is no definition for this alternative in the definitions of the"
                                                      ,"nonterminal" , getName nt_, ". Maybe you misspelled it? Otherwise insert a definition."
                                                      ]
                                act  = wfill ["All rules for the unknown alternative have been ignored."]
                            in ppError (isError _lhsIoptions _me) (getPos con_) mesg pat help act _lhsIverbose
                            {-# LINE 984 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            UndefAlt nt_ con_
                            {-# LINE 990 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 996 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_UndefChild :: NontermIdent ->
                        ConstructorIdent ->
                        Identifier ->
                        T_Error
sem_Error_UndefChild nt_ con_ name_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 280, column 21)
                       _lhsOpp =
                           ({-# LINE 280 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Constructor", getName con_, "of nonterminal" ,getName nt_
                                              , "does not have a nontrivial field named", getName name_ , "."
                                              ]
                                pat   =   "SEM" >#< nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< ppAttr name_ (identifier "<attr>") >#< "= ...")
                                help =  wfill ["There are rules that define or use attributes of field" , getName name_
                                                      ,"in alternative" , getName con_ , "of nonterminal" , getName nt_
                                                      ,", but there is no field with AG-type in the definition of the alternative."
                                                      ,"Maybe you misspelled it? Otherwise insert the field into the definition,"
                                                      ,"or change its type from an HS-type to an AG-type."
                                                      ]
                                act  = wfill ["All rules for the unknown field have been ignored."]
                            in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                            {-# LINE 1024 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            UndefChild nt_ con_ name_
                            {-# LINE 1030 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1036 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_MissingRule :: NontermIdent ->
                         ConstructorIdent ->
                         Identifier ->
                         Identifier ->
                         T_Error
sem_Error_MissingRule nt_ con_ field_ attr_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 295, column 21)
                       _lhsOpp =
                           ({-# LINE 295 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Missing rule for", showAttrDef field_ attr_ , "in alternative"
                                              , getName con_ , "of nonterminal",getName nt_ ,"."
                                              ]
                                pat   =   "SEM" >#< nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                                help  = wfill ["The", showAttrDef field_ attr_, "in alternative", getName con_
                                              , "of nonterminal", getName nt_, "is missing and cannot be inferred"
                                              ,"by a copy rule, so you should add an appropriate rule."
                                              ]
                                act  = wfill ["The value of the attribute has been set to undefined."]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 1063 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            MissingRule nt_ con_ field_ attr_
                            {-# LINE 1069 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1075 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_MissingNamedRule :: NontermIdent ->
                              Identifier ->
                              Identifier ->
                              T_Error
sem_Error_MissingNamedRule nt_ con_ name_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 308, column 23)
                       _lhsOpp =
                           ({-# LINE 308 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Missing rule name ", show name_ , "in alternative"
                                              , getName con_ , "of nonterminal",getName nt_ ,"."
                                              ]
                                pat   =   "SEM" >#< nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< show name_ >#< ": ... = ...")
                                help  = wfill ["There is a dependency on a rule with name ", show name_ , "in alternative"
                                              , getName con_ , "of nonterminal",getName nt_ ,", but no rule has been defined with this name. Maybe you misspelled it?"
                                              ]
                                act  = wfill ["Compilation cannot continue."]
                            in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                            {-# LINE 1100 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            MissingNamedRule nt_ con_ name_
                            {-# LINE 1106 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1112 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_SuperfluousRule :: NontermIdent ->
                             ConstructorIdent ->
                             Identifier ->
                             Identifier ->
                             T_Error
sem_Error_SuperfluousRule nt_ con_ field_ attr_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 320, column 21)
                       _lhsOpp =
                           ({-# LINE 320 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Rule for non-existing", showAttrDef field_ attr_ , "at alternative"
                                              , getName con_ , "of nonterminal",getName nt_, "."
                                              ]
                                pat   =   "SEM" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< ppAttr field_ attr_ >#< "= ...")
                                help =  wfill ["There is a rule for" , showAttrDef field_ attr_ , "in the definitions for alternative" , getName con_
                                              ,"of nonterminal" , getName nt_,  ", but this attribute does not exist. Maybe you misspelled it?"
                                              ,"Otherwise either remove the rule or add an appropriate attribute definition."
                                              ]
                                act  = wfill ["The rule has been ignored."]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 1139 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            SuperfluousRule nt_ con_ field_ attr_
                            {-# LINE 1145 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1151 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_UndefLocal :: NontermIdent ->
                        ConstructorIdent ->
                        Identifier ->
                        T_Error
sem_Error_UndefLocal nt_ con_ var_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 334, column 21)
                       _lhsOpp =
                           ({-# LINE 334 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Undefined local variable or field",getName var_, "at constructor"
                                              , getName con_ , "of nonterminal",getName nt_, "."
                                              ]
                                pat   = "SEM" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< "<field>.<attr> = "
                                                        >#< "..." >#< "@" >|< getName var_ >#< "..." )
                                help =  wfill ["A rule in the definitions for alternative" , getName con_ ,"of nonterminal"
                                              , getName nt_ , "contains a local variable or field name that is not defined. "
                                              ,"Maybe you misspelled it?"
                                              ,"Otherwise either remove the rule or add an appropriate definition."
                                              ]
                                act  = wfill ["The generated program will not run."]
                            in ppError (isError _lhsIoptions _me) (getPos var_) mesg pat help act _lhsIverbose
                            {-# LINE 1179 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            UndefLocal nt_ con_ var_
                            {-# LINE 1185 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1191 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_ChildAsLocal :: NontermIdent ->
                          ConstructorIdent ->
                          Identifier ->
                          T_Error
sem_Error_ChildAsLocal nt_ con_ var_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 349, column 21)
                       _lhsOpp =
                           ({-# LINE 349 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Nontrivial field ",getName var_, "is used as local at constructor"
                                              , getName con_ , "of nonterminal",getName nt_, "."
                                              ]
                                pat   = "SEM" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< "... = "
                                                        >#< "..." >#< "@" >|< getName var_ >#< "..." )
                                help =  wfill ["A rule in the definitions for alternative" , getName con_ ,"of nonterminal"
                                              , getName nt_ , "contains a nontrivial field name", getName var_, "."
                                              ,"You should use @", getName var_, ".self instead, where self is a SELF-attribute."
                                              ]
                                act  = wfill ["The generated program probably contains a type error or has undefined variables."]
                            in ppError (isError _lhsIoptions _me) (getPos var_) mesg pat help act _lhsIverbose
                            {-# LINE 1218 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            ChildAsLocal nt_ con_ var_
                            {-# LINE 1224 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1230 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_UndefAttr :: NontermIdent ->
                       ConstructorIdent ->
                       Identifier ->
                       Identifier ->
                       Bool ->
                       T_Error
sem_Error_UndefAttr nt_ con_ field_ attr_ isOut_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 363, column 21)
                       _lhsOpp =
                           ({-# LINE 363 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Undefined"
                                              , if isOut_
                                                then showAttrDef field_ attr_
                                                else showAttrUse field_ attr_
                                              , "at constructor"
                                              , getName con_ , "of nonterminal",getName nt_, "."
                                              ]
                                pat   = "SEM" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< "<field>.<attr> = "
                                                        >#< "..." >#< ppAttrUse field_ attr_ >#< "...")
                                help =  wfill ["A rule in the definitions for alternative" , getName con_ ,"of nonterminal"
                                              ,getName  nt_ , "contains an attribute that is not defined"
                                              ,"Maybe you misspelled it?"
                                              ,"Otherwise either remove the rule or add an appropriate attribute definition."
                                              ]
                                act  = wfill ["The generated program will not run."]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 1264 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            UndefAttr nt_ con_ field_ attr_ isOut_
                            {-# LINE 1270 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1276 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_Cyclic :: NontermIdent ->
                    (Maybe ConstructorIdent) ->
                    ([String]) ->
                    T_Error
sem_Error_Cyclic nt_ mbCon_ verts_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 391, column 21)
                       _lhsOpp =
                           ({-# LINE 391 "./src-ag/PrintErrorMessages.ag" #-}
                            let pos  = getPos nt_
                                mesg = text "Circular dependency for nonterminal" >#< getName nt_
                                       >#< ( case mbCon_ of
                                               Nothing  -> empty
                                               Just con -> text "and constructor" >#< con
                                           )
                                       >#< ( case verts_ of
                                                 v : _ -> text "including vertex" >#< text v
                                                 _     -> empty
                                           )
                                pat  = text "cyclic rule definition"
                                help = hlist (text "The following attributes are all cyclic: " : map text verts_)
                                act  = wfill ["code cannot be generated until the cycle is removed."]
                            in ppError (isError _lhsIoptions _me) pos mesg pat help act False
                            {-# LINE 1305 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            Cyclic nt_ mbCon_ verts_
                            {-# LINE 1311 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1317 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_CyclicSet :: Identifier ->
                       T_Error
sem_Error_CyclicSet name_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 382, column 21)
                       _lhsOpp =
                           ({-# LINE 382 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Cyclic definition for nonterminal set", getName name_]
                                pat   = "SET" >#< getName name_ >#< "=" >#< "..." >#< getName name_ >#< "..."
                                help =  wfill ["The defintion for a nonterminal set named" , getName name_
                                              ,"directly or indirectly refers to itself."
                                              ,"Adapt the definition of the nonterminal set, to remove the cyclic dependency."
                                              ]
                                act  = wfill ["The nonterminal set", getName name_, "is considered to be empty."]
                            in ppError (isError _lhsIoptions _me) (getPos name_) mesg pat help act _lhsIverbose
                            {-# LINE 1338 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            CyclicSet name_
                            {-# LINE 1344 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1350 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_CustomError :: Bool ->
                         Pos ->
                         PP_Doc ->
                         T_Error
sem_Error_CustomError isWarning_ pos_ mesg_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 406, column 21)
                       _lhsOpp =
                           ({-# LINE 406 "./src-ag/PrintErrorMessages.ag" #-}
                            let pat   =  text "unknown"
                                help = wfill ["not available."]
                                act  = wfill ["unknown"]
                            in ppError (isError _lhsIoptions _me) pos_ mesg_ pat help act False
                            {-# LINE 1369 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            CustomError isWarning_ pos_ mesg_
                            {-# LINE 1375 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1381 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_LocalCirc :: NontermIdent ->
                       ConstructorIdent ->
                       Identifier ->
                       Bool ->
                       ([String]) ->
                       T_Error
sem_Error_LocalCirc nt_ con_ attr_ o_visit_ path_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 411, column 21)
                       _lhsOpp =
                           ({-# LINE 411 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Circular dependency for local attribute", getName attr_
                                              , "of alternative", getName con_, "of nonterminal", getName nt_]
                                pat   = "SEM" >#< getName nt_
                                        >-< indent 2 ("|" >#< getName con_ >#< "loc." >|< getName attr_ >#< "="
                                                          >#< "..." >#< "@loc." >|< getName attr_ >#< "...")
                                help  = if null path_
                                        then text "the definition is directly circular"
                                        else hlist ("The following attributes are involved in the cycle:": path_)
                                act   | o_visit_ = text "An unoptimized version was generated. It might hang when run."
                                      | otherwise = text "The generated program might hang when run."
                            in ppError (isError _lhsIoptions _me) (getPos (attr_)) mesg pat help act _lhsIverbose
                            {-# LINE 1409 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            LocalCirc nt_ con_ attr_ o_visit_ path_
                            {-# LINE 1415 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1421 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_InstCirc :: NontermIdent ->
                      ConstructorIdent ->
                      Identifier ->
                      Bool ->
                      ([String]) ->
                      T_Error
sem_Error_InstCirc nt_ con_ attr_ o_visit_ path_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 423, column 21)
                       _lhsOpp =
                           ({-# LINE 423 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Circular dependency for inst attribute", getName attr_
                                              , "of alternative", getName con_, "of nonterminal", getName nt_]
                                pat   = "SEM" >#< getName nt_
                                        >-< indent 2 ("|" >#< getName con_ >#< "inst." >|< getName attr_ >#< "="
                                                          >#< "..." >#< "@s.<some attribte>" >#< "...")
                                help  = if null path_
                                        then text "the definition is directly circular"
                                        else hlist ("The following attributes are involved in the cycle:": path_)
                                act   | o_visit_ = text "An unoptimized version was generated. It might hang when run."
                                      | otherwise = text "The generated program might hang when run."
                            in ppError (isError _lhsIoptions _me) (getPos (attr_)) mesg pat help act _lhsIverbose
                            {-# LINE 1449 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            InstCirc nt_ con_ attr_ o_visit_ path_
                            {-# LINE 1455 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1461 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DirectCirc :: NontermIdent ->
                        Bool ->
                        ([((Identifier,Identifier),[String],[String])]) ->
                        T_Error
sem_Error_DirectCirc nt_ o_visit_ cyclic_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 435, column 21)
                       _lhsOpp =
                           ({-# LINE 435 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["In nonterminal", getName nt_, "synthesized and inherited attributes are mutually dependent" ]
                                        >-< vlist (map showEdge cyclic_)
                                pat   = text ""
                                help  = vlist (map showEdgeLong cyclic_)
                                act   | o_visit_ = text "An unoptimized version was generated. It might hang when run."
                                      | otherwise = text "The generated program might hang when run."
                            in ppError (isError _lhsIoptions _me) noPos mesg pat help act _lhsIverbose
                            {-# LINE 1483 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DirectCirc nt_ o_visit_ cyclic_
                            {-# LINE 1489 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1495 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_InducedCirc :: NontermIdent ->
                         CInterface ->
                         ([((Identifier,Identifier),[String],[String])]) ->
                         T_Error
sem_Error_InducedCirc nt_ cinter_ cyclic_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 443, column 21)
                       _lhsOpp =
                           ({-# LINE 443 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["After scheduling, in nonterminal", getName nt_, "synthesized and inherited attributes have an INDUCED mutual dependency" ]
                                        >-< vlist (map showEdge cyclic_)
                                pat   = text ""
                                showInter (CInterface segs) = concat (snd (mapAccumL (\i c -> (succ i :: Integer,("visit " ++ show i) : map ind (showsSegment c))) 0 segs))
                                help  = vlist (("Interface for nonterminal " ++ getName nt_ ++ ":") : map ind (showInter cinter_))
                                        >-< vlist (map showEdgeLong cyclic_)
                                act   = text "An unoptimized version was generated. It might hang when run."
                            in ppError (isError _lhsIoptions _me) noPos mesg pat help act _lhsIverbose
                            {-# LINE 1518 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            InducedCirc nt_ cinter_ cyclic_
                            {-# LINE 1524 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1530 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_MissingTypeSig :: NontermIdent ->
                            ConstructorIdent ->
                            Identifier ->
                            T_Error
sem_Error_MissingTypeSig nt_ con_ attr_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 452, column 21)
                       _lhsOpp =
                           ({-# LINE 452 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg = wfill ["Type signature needed, but not found for", showAttrDef _LOC attr_ , "in alternative"
                                              , getName con_ , "of nonterminal",getName nt_ ,"."
                                              ]>-<
                                        wfill ["Location:", (showPos attr_),"."]
                                pat   =   "SEM" >#< nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< ": ...")
                                help  = wfill ["The", showAttrDef _LOC attr_, "in alternative", getName con_
                                              ,"of nonterminal", getName nt_, "is needed in two separate visits to", getName nt_
                                              ,"so its type is needed to generate type signatures."
                                              ,"Please supply its type."
                                              ]
                                act  = wfill ["The type signatures of semantic functions are not generated."]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 1558 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            MissingTypeSig nt_ con_ attr_
                            {-# LINE 1564 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1570 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_MissingInstSig :: NontermIdent ->
                            ConstructorIdent ->
                            Identifier ->
                            T_Error
sem_Error_MissingInstSig nt_ con_ attr_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 466, column 21)
                       _lhsOpp =
                           ({-# LINE 466 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg = wfill ["Type signature needed, but not found for", showAttrDef _INST attr_ , "in alternative"
                                              , getName con_ , "of nonterminal",getName nt_ ,"."
                                              ]>-<
                                        wfill ["Location:", (showPos attr_),"."]
                                pat   = "SEM" >#< nt_
                                          >-< indent 2 ("|" >#< getName con_ >#< ppAttr _INST attr_ >#< ": ...")
                                help  = wfill ["The", showAttrDef _INST attr_, "in alternative", getName con_
                                              ,"of nonterminal", getName nt_, "is a non-terminal attribute, so "
                                              ,"its type is needed to attribute its value."
                                              ,"Please supply its type."
                                              ]
                                act  = wfill ["It is not possible to proceed without this signature."]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 1598 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            MissingInstSig nt_ con_ attr_
                            {-# LINE 1604 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1610 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_DupUnique :: NontermIdent ->
                       ConstructorIdent ->
                       Identifier ->
                       T_Error
sem_Error_DupUnique nt_ con_ attr_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 496, column 21)
                       _lhsOpp =
                           ({-# LINE 496 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["At constructor",getName con_, "of nonterminal", getName nt_, "there are two or more unique-attribute signatures for"
                                              ,showAttrDef _LOC attr_,"."
                                              ]  >-<
                                        wfill ["First signature:", (showPos attr_),"."]
                                pat   =   "SEM" >#< getName nt_
                                      >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< " : UNIQUEREF ...")
                                      >-< indent 2 ("|" >#< getName con_ >#< ppAttr _LOC attr_ >#< " : UNIQUEREF ...")
                                help =  wfill ["In the rules for alternative" , getName con_ , "of nonterminal" , getName nt_
                                                      ,", there is more than one unique-attribute signature for the" , showAttrDef _LOC attr_
                                                      ,". You should remove enough of them to make all unique-signatures for alternative"
                                                      ,getName con_ , "of nonterminal " ,getName  nt_ , "unique."
                                                      ]
                                act  = wfill ["Unpredicatable sharing of unique numbers may occur."]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 1639 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            DupUnique nt_ con_ attr_
                            {-# LINE 1645 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1651 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_MissingUnique :: NontermIdent ->
                           Identifier ->
                           T_Error
sem_Error_MissingUnique nt_ attr_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 480, column 21)
                       _lhsOpp =
                           ({-# LINE 480 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Missing unique counter (chained attribute)"
                                              , getName attr_
                                              , "at nonterminal"
                                              , getName nt_, "."
                                              ]
                                pat   = "ATTR" >#< getName nt_ >#< "[ |" >#< getName attr_ >#< " : ... | ]"
                                help =  wfill ["A unique attribute signature in a constructor for nonterminal" , getName nt_
                                              , "refers to an unique counter (chained attribute) named "
                                              , getName attr_
                                              ,"Maybe you misspelled it?"
                                              ,"Otherwise either remove the signature or add an appropriate attribute definition."
                                              ]
                                act  = wfill ["It is not possible to proceed without this declaration."]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 1679 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            MissingUnique nt_ attr_
                            {-# LINE 1685 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1691 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_MissingSyn :: NontermIdent ->
                        Identifier ->
                        T_Error
sem_Error_MissingSyn nt_ attr_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 513, column 20)
                       _lhsOpp =
                           ({-# LINE 513 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = wfill ["Missing synthesized attribute"
                                              , getName attr_
                                              , "at nonterminal"
                                              , getName nt_, "."
                                              ]
                                pat   = "ATTR" >#< getName nt_ >#< "[ | | " >#< getName attr_ >#< " : ... ]"
                                help =  wfill ["An augment rule for a constructor for nonterminal" , getName nt_
                                              , "refers to a synthesized attribute named "
                                              , getName attr_
                                              ,"Maybe you misspelled it?"
                                              ,"Otherwise add an appropriate attribute definition."
                                              ]
                                act  = wfill ["It is not possible to proceed without this declaration."]
                            in ppError (isError _lhsIoptions _me) (getPos attr_) mesg pat help act _lhsIverbose
                            {-# LINE 1719 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            MissingSyn nt_ attr_
                            {-# LINE 1725 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1731 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_IncompatibleVisitKind :: Identifier ->
                                   VisitIdentifier ->
                                   VisitKind ->
                                   VisitKind ->
                                   T_Error
sem_Error_IncompatibleVisitKind child_ vis_ from_ to_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 529, column 20)
                       _lhsOpp =
                           ({-# LINE 529 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = "visit" >#< vis_ >#< "of child" >#< child_ >#< " with kind" >#< show to_ >#< " cannot be called from a visit with kind " >#< show from_
                                pat   = empty
                                help  = empty
                                act   = text "It is not possible to proceed without fixing this kind error."
                            in ppError (isError _lhsIoptions _me) (getPos child_) mesg pat help act _lhsIverbose
                            {-# LINE 1752 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            IncompatibleVisitKind child_ vis_ from_ to_
                            {-# LINE 1758 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1764 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_IncompatibleRuleKind :: Identifier ->
                                  VisitKind ->
                                  T_Error
sem_Error_IncompatibleRuleKind rule_ kind_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 535, column 20)
                       _lhsOpp =
                           ({-# LINE 535 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = "rule" >#< rule_ >#< "cannot be called from a visit with kind " >#< show kind_
                                pat   = empty
                                help  = empty
                                act   = text "It is not possible to proceed without fixing this kind error."
                            in ppError (isError _lhsIoptions _me) (getPos rule_) mesg pat help act _lhsIverbose
                            {-# LINE 1783 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            IncompatibleRuleKind rule_ kind_
                            {-# LINE 1789 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1795 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
sem_Error_IncompatibleAttachKind :: Identifier ->
                                    VisitKind ->
                                    T_Error
sem_Error_IncompatibleAttachKind child_ kind_ =
    (T_Error (\ _lhsIoptions
                _lhsIverbose ->
                  (let _lhsOpp :: PP_Doc
                       _lhsOme :: Error
                       -- "./src-ag/PrintErrorMessages.ag"(line 542, column 20)
                       _lhsOpp =
                           ({-# LINE 542 "./src-ag/PrintErrorMessages.ag" #-}
                            let mesg  = "child" >#< child_ >#< "cannot be called from a visit with kind " >#< show kind_
                                pat   = empty
                                help  = empty
                                act   = text "It is not possible to proceed without fixing this kind error."
                            in ppError (isError _lhsIoptions _me) (getPos child_) mesg pat help act _lhsIverbose
                            {-# LINE 1814 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _me =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            IncompatibleAttachKind child_ kind_
                            {-# LINE 1820 "dist/build/PrintErrorMessages.hs" #-}
                            )
                       -- self rule
                       _lhsOme =
                           ({-# LINE 69 "./src-ag/PrintErrorMessages.ag" #-}
                            _me
                            {-# LINE 1826 "dist/build/PrintErrorMessages.hs" #-}
                            )
                   in  ( _lhsOme,_lhsOpp))))
-- Errors ------------------------------------------------------
{-
   visit 0:
      inherited attributes:
         dups                 : [String]
         options              : Options
      synthesized attribute:
         pp                   : PP_Doc
   alternatives:
      alternative Cons:
         child hd             : Error 
         child tl             : Errors 
         visit 0:
            local verbose     : _
            local str         : _
      alternative Nil:
         visit 0:
            local verbose     : _
-}
-- cata
sem_Errors :: Errors ->
              T_Errors
sem_Errors list =
    (Prelude.foldr sem_Errors_Cons sem_Errors_Nil (Prelude.map sem_Error list))
-- semantic domain
newtype T_Errors = T_Errors (([String]) ->
                             Options ->
                             ( PP_Doc))
data Inh_Errors = Inh_Errors {dups_Inh_Errors :: ([String]),options_Inh_Errors :: Options}
data Syn_Errors = Syn_Errors {pp_Syn_Errors :: PP_Doc}
wrap_Errors :: T_Errors ->
               Inh_Errors ->
               Syn_Errors
wrap_Errors (T_Errors sem) (Inh_Errors _lhsIdups _lhsIoptions) =
    (let ( _lhsOpp) = sem _lhsIdups _lhsIoptions
     in  (Syn_Errors _lhsOpp))
sem_Errors_Cons :: T_Error ->
                   T_Errors ->
                   T_Errors
sem_Errors_Cons (T_Error hd_) (T_Errors tl_) =
    (T_Errors (\ _lhsIdups
                 _lhsIoptions ->
                   (let _lhsOpp :: PP_Doc
                        _tlOdups :: ([String])
                        _hdOoptions :: Options
                        _hdOverbose :: Bool
                        _tlOoptions :: Options
                        _hdIme :: Error
                        _hdIpp :: PP_Doc
                        _tlIpp :: PP_Doc
                        -- "./src-ag/PrintErrorMessages.ag"(line 76, column 8)
                        _verbose =
                            ({-# LINE 76 "./src-ag/PrintErrorMessages.ag" #-}
                             verbose _lhsIoptions
                             {-# LINE 1883 "dist/build/PrintErrorMessages.hs" #-}
                             )
                        -- "./src-ag/PrintErrorMessages.ag"(line 77, column 11)
                        _str =
                            ({-# LINE 77 "./src-ag/PrintErrorMessages.ag" #-}
                             disp _hdIpp 5000 ""
                             {-# LINE 1889 "dist/build/PrintErrorMessages.hs" #-}
                             )
                        -- "./src-ag/PrintErrorMessages.ag"(line 79, column 11)
                        _lhsOpp =
                            ({-# LINE 79 "./src-ag/PrintErrorMessages.ag" #-}
                             if _str     `elem` _lhsIdups
                             then _tlIpp
                             else _hdIpp >-< _tlIpp
                             {-# LINE 1897 "dist/build/PrintErrorMessages.hs" #-}
                             )
                        -- "./src-ag/PrintErrorMessages.ag"(line 82, column 11)
                        _tlOdups =
                            ({-# LINE 82 "./src-ag/PrintErrorMessages.ag" #-}
                             _str     : _lhsIdups
                             {-# LINE 1903 "dist/build/PrintErrorMessages.hs" #-}
                             )
                        -- copy rule (down)
                        _hdOoptions =
                            ({-# LINE 68 "./src-ag/PrintErrorMessages.ag" #-}
                             _lhsIoptions
                             {-# LINE 1909 "dist/build/PrintErrorMessages.hs" #-}
                             )
                        -- copy rule (from local)
                        _hdOverbose =
                            ({-# LINE 68 "./src-ag/PrintErrorMessages.ag" #-}
                             _verbose
                             {-# LINE 1915 "dist/build/PrintErrorMessages.hs" #-}
                             )
                        -- copy rule (down)
                        _tlOoptions =
                            ({-# LINE 72 "./src-ag/PrintErrorMessages.ag" #-}
                             _lhsIoptions
                             {-# LINE 1921 "dist/build/PrintErrorMessages.hs" #-}
                             )
                        ( _hdIme,_hdIpp) =
                            hd_ _hdOoptions _hdOverbose
                        ( _tlIpp) =
                            tl_ _tlOdups _tlOoptions
                    in  ( _lhsOpp))))
sem_Errors_Nil :: T_Errors
sem_Errors_Nil =
    (T_Errors (\ _lhsIdups
                 _lhsIoptions ->
                   (let _lhsOpp :: PP_Doc
                        -- "./src-ag/PrintErrorMessages.ag"(line 76, column 8)
                        _verbose =
                            ({-# LINE 76 "./src-ag/PrintErrorMessages.ag" #-}
                             verbose _lhsIoptions
                             {-# LINE 1937 "dist/build/PrintErrorMessages.hs" #-}
                             )
                        -- "./src-ag/PrintErrorMessages.ag"(line 83, column 11)
                        _lhsOpp =
                            ({-# LINE 83 "./src-ag/PrintErrorMessages.ag" #-}
                             text ""
                             {-# LINE 1943 "dist/build/PrintErrorMessages.hs" #-}
                             )
                    in  ( _lhsOpp))))